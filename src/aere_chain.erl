-module(aere_chain).

-include("../apps/aecontract/include/aecontract.hrl").
-include("../apps/aecore/include/blocks.hrl").
-include("aere_repl.hrl").

-define(cid(__x__), {'@ct', __x__}).
-define(hsh(__x__), {'#', __x__}).
-define(sig(__x__), {'$sg', __x__}).
-define(oid(__x__), {'@ok', __x__}).
-define(qid(__x__), {'@oq', __x__}).


-export([state/0, state/1, new_account/2, create_contract/5, call_contract/7]).

state()  -> get(the_state).
state(S) -> put(the_state, S).

new_account(Balance, State) ->
    {PubKey, PrivKey} = new_key_pair(),
    State1            = insert_key_pair(PubKey, PrivKey, State),
    State2            = set_account(aec_accounts:new(PubKey, Balance), State1),
    {PubKey, State2}.

new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

insert_key_pair(Pub, Priv, S) ->
    Old = key_pairs(S),
    S#{key_pairs => Old#{Pub => Priv}}.
key_pairs(S) -> maps:get(key_pairs, S, #{}).

set_account(Account, State) ->
    Trees   = trees(State),
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    State#{trees => aec_trees:set_accounts(Trees, AccTree)}.

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new()).

create_contract(Owner, Code, Args, Options, S) ->
    Nonce       = next_nonce(Owner, S),
    CallData    = make_calldata_from_code(Code, init, Args, Options),
    TxOptions    = #{ nonce       => Nonce
                    , code        => Code
                    , call_data   => CallData
                    , gas         => Options#options.gas
                    , amount      => Options#options.call_value
                    , vm_version  => aere_version:vm_version(fate)
                    , abi_version => aere_version:abi_version(fate)
                    },
    CreateTx    = create_tx(Owner, TxOptions, S),
    Height      = Options#options.height,
    PrivKey     = priv_key(Owner, S),
    S1          = case sign_and_apply_transaction(CreateTx, PrivKey, S, Height) of
                      {ok, TmpS}        -> TmpS;
                      {error, R, _TmpS} -> error(R)
                  end,
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    CallKey     = aect_call:id(Owner, Nonce, ContractKey),
    CallTree    = calls(S1),
    Call        = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
    ReturnValue = aect_call:return_value(Call),
    ReturnType  = aect_call:return_type(Call),
    []          = [error({failed_contract_create, ReturnValue}) || ReturnType =/= ok],
    {{ContractKey, aect_call:gas_used(Call)}, S1}.

call_contract(Caller, ContractKey, Fun, Type, Args, Options, S) ->
    Calldata = make_calldata_from_id(ContractKey, Fun, Args, Options, S),
    call_contract_with_calldata(Caller, ContractKey, Type, Calldata, Options, S).

call_contract_with_calldata(Caller, ContractKey, Type, Calldata, Options, S) ->
    Nonce    = next_nonce(Caller, S),
    CallTx   = call_tx(Caller, ContractKey,
                       #{ nonce       => Nonce
                        , call_data   => Calldata
                        , gas         => Options#options.gas
                        , amount      => Options#options.call_value
                        , abi_version => aere_version:abi_version(fate)
                        }, S),
    Height   = Options#options.height,
    PrivKey  = priv_key(Caller, S),
    case sign_and_apply_transaction(CallTx, PrivKey, S, Height) of
        {ok, S1} ->
            CallKey  = aect_call:id(Caller, Nonce, ContractKey),
            CallTree = calls(S1),
            Call     = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
            Result   = call_result(Type, Call),
            {{Result, aect_call:gas_used(Call)}, S1};
        {error, R, S1} ->
            {{error, R}, S1}
    end.

call_result(Type, Call) ->
    case aect_call:return_type(Call) of
        ok     ->
            Res = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            case aere_response:decode(Res, Type) of
                {variant, [0,1], 0, {}} when element(1, Type) =:= option ->
                    none;
                {variant, [0,1], 1, {Decoded}} when element(1, Type) =:= option ->
                    {some, Decoded};
                Decoded ->
                    Decoded
            end;
        error  ->
            {error, aect_call:return_value(Call)};
        revert ->
            Res = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            {revert, aere_response:decode(Res)}
    end.

sign_and_apply_transaction(Tx, PrivKey, S = #{trees := Trees}, Height) ->
    SignedTx = sign_tx(Tx, PrivKey),
    Env      = default_tx_env(Height),
    case aec_block_micro_candidate:apply_block_txs_strict([SignedTx], Trees, Env) of
        {ok, [SignedTx], Trees1, _} ->
            {ok, S#{trees => Trees1}};
        {error, R} ->
            {error, R, S}
    end.

-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).
default_tx_env(Height) ->
    aetx_env:set_beneficiary(aetx_env:tx_env(Height), ?BENEFICIARY_PUBKEY).


make_calldata_from_id(Id, Fun, Args, Opts, State) ->
    {{value, C}, _S} = lookup_contract_by_id(Id, State),
    make_calldata_from_code(aect_contracts:code(C), Fun, Args, Opts).

lookup_contract_by_id(ContractKey, S) ->
    Contracts = aec_trees:contracts(trees(S)),
    X         = aect_state_tree:lookup_contract(ContractKey, Contracts),
    {X, S}.



make_calldata_from_code(Code, Fun, Args, Opts) when is_atom(Fun) ->
    make_calldata_from_code(Code, atom_to_binary(Fun, latin1), Args, Opts);
make_calldata_from_code(_Code, Fun, Args, _Opts) when is_binary(Fun) ->
    Args1 = format_fate_args(if is_tuple(Args) -> Args;
                                true -> {Args}
                             end),
    FunctionId = make_fate_function_id(Fun),
    aeb_fate_encoding:serialize(aere_response:encode({FunctionId, Args1})).


make_fate_function_id(FunctionName) when is_binary(FunctionName) ->
    aeb_fate_code:symbol_identifier(FunctionName).

format_fate_args(?cid(B)) ->
    {contract, B};
format_fate_args(?hsh(B)) ->
    {bytes, B};
format_fate_args(?sig(B)) ->
    {bytes, B};
format_fate_args(?oid(B)) ->
    {oracle, B};
format_fate_args(?qid(B)) ->
    {oracle_query, B};
format_fate_args(<<_:256>> = B) ->
    {address, B}; %% Assume it is an address
format_fate_args({bytes, B}) ->
    {bytes, B};
format_fate_args([H|T]) ->
    [format_fate_args(H) | format_fate_args(T)];
format_fate_args(T) when is_tuple(T) ->
    list_to_tuple(format_fate_args(tuple_to_list(T)));
format_fate_args(M) when is_map(M) ->
    maps:from_list(format_fate_args(maps:to_list(M)));
format_fate_args(X) ->
    X.


next_nonce(PubKey, S) ->
    Account = aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(S))),
    aec_accounts:nonce(Account) + 1.

priv_key(PubKey, State) ->
    maps:get(PubKey, key_pairs(State)).

calls(State) ->
    aec_trees:calls(trees(State)).

create_tx(Owner, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(Owner, State), Spec0),
    {ok, Tx} = aect_create_tx:new(Spec),
    Tx.


call_tx(PubKey, ContractKey, Spec0, State) ->
    Spec = maps:merge(call_tx_default_spec(PubKey, ContractKey, State), Spec0),
    {ok, Tx} = aect_call_tx:new(Spec),
    Tx.

call_tx_default_spec(PubKey, ContractKey, State) ->
    #{ fee         => 600000 * min_gas_price()
     , contract_id => aeser_id:create(contract, ContractKey)
     , caller_id   => aeser_id:create(account, PubKey)
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , abi_version => aere_version:abi_version(fate)
     , amount      => 100000
     , gas         => 100000
     , gas_price   => 1 * min_gas_price()
     , ttl         => 0
     }.

create_tx_default_spec(PubKey, State) ->
    #{ fee         => 1000000 * min_gas_price()
     , owner_id    => aeser_id:create(account, PubKey)
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , vm_version  => aere_version:vm_version(fate)
     , abi_version => aere_version:abi_version(fate)
     , deposit     => 2137
     , amount      => 100000
     , gas         => 100000
     , gas_price   => 1 * min_gas_price()
     , ttl         => 0
     }.

min_gas_price() ->
    max(aec_governance:minimum_gas_price(1), % latest prototocol on height 1
    aec_tx_pool:minimum_miner_gas_price()).



-define(VALID_PRIVK(K), byte_size(K) =:= 64).


sign_tx(Tx, PrivKey) ->
    sign_tx(Tx, PrivKey, false).

sign_tx(Tx, PrivKey, SignHash) when is_binary(PrivKey) ->
    sign_tx(Tx, [PrivKey], SignHash);
sign_tx(Tx, PrivKeys, SignHash) when is_list(PrivKeys) ->
    Bin0 = aetx:serialize_to_binary(Tx),
    Bin =
        case SignHash of
            true  -> aec_hash:hash(signed_tx, Bin0);
            false -> Bin0
        end,
    BinForNetwork = aec_governance:add_network_id(Bin),
    case lists:filter(fun(PrivKey) -> not (?VALID_PRIVK(PrivKey)) end, PrivKeys) of
        [_|_]=BrokenKeys -> erlang:error({invalid_priv_key, BrokenKeys});
        [] -> pass
    end,
    Signatures = [ enacl:sign_detached(BinForNetwork, PrivKey) || PrivKey <- PrivKeys ],
    aetx_sign:new(Tx, Signatures).

