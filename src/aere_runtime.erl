-module(aere_runtime).

-include("../apps/aecontract/include/aecontract.hrl").
-include("../apps/aecore/include/blocks.hrl").
-include("../apps/aecontract/test/include/aect_sophia_vsn.hrl").

-define(cid(__x__), {'@ct', __x__}).
-define(hsh(__x__), {'#', __x__}).
-define(sig(__x__), {'$sg', __x__}).
-define(oid(__x__), {'@ok', __x__}).
-define(qid(__x__), {'@oq', __x__}).


-export([state/0, state/1, new_account/2, create_contract/5, call_contract/6]).

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
    set_trees(aec_trees:set_accounts(Trees, AccTree), State).

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new()).

set_trees(Trees, S) ->
    S#{trees => Trees}.



create_contract(Owner, Code, Args, Options, S) ->
    create_contract(Owner, Code, Args, Options, S, false, false).

create_contract(Owner, Code, Args, Options, S, TxFail, InitFail) ->
    Nonce       = next_nonce(Owner, S),
    CallData    = make_calldata_from_code(Code, init, Args),
    Options1    = maps:merge(#{ nonce => Nonce
                              , code => Code
                              , call_data => CallData},
                             maps:without([height, return_return_value, return_gas_used], Options)),
    CreateTx    = create_tx(Owner, Options1, S),
    Height      = maps:get(height, Options, 1),
    PrivKey     = priv_key(Owner, S),
    S1          = case sign_and_apply_transaction(CreateTx, PrivKey, S, Height) of
                      {ok, TmpS} when not TxFail -> TmpS;
                      {ok,_TmpS} when TxFail -> error({error, succeeded});
                      {error, R,_TmpS} when not TxFail -> error(R);
                      {error, R, TmpS} when TxFail -> throw({ok, R, TmpS})
                  end,
    ContractKey = aect_contracts:compute_contract_pubkey(Owner, Nonce),
    CallKey     = aect_call:id(Owner, Nonce, ContractKey),
    CallTree    = calls(S1),
    Call        = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
    Result0     = ContractKey,
    ReturnValue = aect_call:return_value(Call),
    ReturnType  = aect_call:return_type(Call),
    []          = [error({failed_contract_create, ReturnValue})
                   || (InitFail andalso ReturnType =:= ok)
                          orelse (not InitFail andalso ReturnType =/= ok)],
    Result1     =
        case maps:get(return_return_value, Options, false) of
            false -> Result0;
            true  -> {Result0, {ReturnType, ReturnValue}}
        end,
    case maps:get(return_gas_used, Options, false) of
        false -> {Result1, S1};
        true  -> {{Result1, aect_call:gas_used(Call)}, S1}
    end.

call_contract(Caller, ContractKey, Fun, Type, Args, S) ->
    call_contract(Caller, ContractKey, Fun, Type, Args, #{}, S).

call_contract(Caller, ContractKey, Fun, Type, Args, Options, S) ->
    Calldata = make_calldata_from_id(ContractKey, Fun, Args, S),
    call_contract_with_calldata(Caller, ContractKey, Type, Calldata, Options, S).

call_contract_with_calldata(Caller, ContractKey, Type, Calldata, Options, S) ->
    Nonce    = next_nonce(Caller, S),
    CallTx   = call_tx(Caller, ContractKey,
                maps:merge(
                #{ nonce       => Nonce
                 , abi_version => aere_version:latest_sophia_abi_version()
                 , call_data   => Calldata
                 , fee         => maps:get(fee, Options, 1000000 * min_gas_price())
                 , amount      => 0
                 , gas         => 140000
                 }, maps:without([height, return_gas_used, return_logs], Options)), S),
    Height   = maps:get(height, Options, 1),
    PrivKey  = priv_key(Caller, S),
    case sign_and_apply_transaction(CallTx, PrivKey, S, Height) of
        {ok, S1} ->
            CallKey  = aect_call:id(Caller, Nonce, ContractKey),
            CallTree = calls(S1),
            Call     = aect_call_state_tree:get_call(ContractKey, CallKey, CallTree),
            {_, Tx}  = aetx:specialize_type(CallTx),
            ABI      = aect_call_tx:abi_version(Tx),
            Result   = call_result(ABI, Type, Call),
            case maps:get(return_gas_used, Options, false) of
                false -> {Result, S1};
                true  -> {{Result, aect_call:gas_used(Call)}, S1}
            end;
        {error, R, S1} ->
            {{error, R}, S1}
    end.

call_result(?ABI_AEVM_SOPHIA_1, Type, Call) ->
    case aect_call:return_type(Call) of
        error  ->
            {error, aect_call:return_value(Call)};
        ok ->
            {ok, Res} = aeb_heap:from_binary(Type, aect_call:return_value(Call)),
            Res;
        revert ->
            {ok, Res} = aeb_heap:from_binary(string, aect_call:return_value(Call)),
            {revert, Res}
    end;
call_result(?ABI_FATE_SOPHIA_1, Type, Call) ->
    case aect_call:return_type(Call) of
        ok     ->
            Res = aeb_fate_encoding:deserialize(aect_call:return_value(Call)),
            case aefate_test_utils:decode(Res, Type) of
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
            {revert, aefate_test_utils:decode(Res)}
    end.

sign_and_apply_transaction(Tx, PrivKey, S1, Height) ->
    SignedTx = sign_tx(Tx, PrivKey),
    Trees    = trees(S1),
    Env      = default_tx_env(Height),
    case aec_block_micro_candidate:apply_block_txs_strict([SignedTx], Trees, Env) of
        {ok, [SignedTx], Trees1, _} ->
            S2 = set_trees(Trees1, S1),
            {ok, S2};
        {error, R} ->
            {error, R, S1}
    end.

-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).
default_tx_env(Height) ->
    aetx_env:set_beneficiary(aetx_env:tx_env(Height), ?BENEFICIARY_PUBKEY).


make_calldata_from_id(Id, Fun, Args, State) ->
    {{value, C}, _S} = lookup_contract_by_id(Id, State),
    make_calldata_from_code(aect_contracts:code(C), Fun, Args).

lookup_contract_by_id(ContractKey, S) ->
    Contracts = aec_trees:contracts(trees(S)),
    X         = aect_state_tree:lookup_contract(ContractKey, Contracts),
    {X, S}.



make_calldata_from_code(Code, Fun, Args) when is_atom(Fun) ->
    make_calldata_from_code(Code, atom_to_binary(Fun, latin1), Args);
make_calldata_from_code(Code, Fun, Args) when is_binary(Fun) ->
    case aere_version:latest_sophia_abi_version() of
        ?ABI_AEVM_SOPHIA_1 ->
            #{type_info := TypeInfo} = aect_sophia:deserialize(Code),
            case aeb_aevm_abi:type_hash_from_function_name(Fun, TypeInfo) of
                {ok, <<FunHashInt:256>>} ->
                    Args1 = format_aevm_args(if is_tuple(Args) -> Args;
                                                true -> {Args}
                                             end),
                    aeb_heap:to_binary({FunHashInt, Args1});
                {error, _} = Err -> error({bad_function, Fun, Err})
            end;
        ?ABI_FATE_SOPHIA_1 ->
            %% TODO: Move this into aefa_fate
            Args1 = format_fate_args(if is_tuple(Args) -> Args;
                                        true -> {Args}
                                     end),
            FunctionId = make_fate_function_id(Fun),
            aeb_fate_encoding:serialize(aefate_test_utils:encode({FunctionId, Args1}))
    end.


format_aevm_args(?cid(<<N:256>>)) -> N;
format_aevm_args(?hsh(<<N:256>>)) -> N;
format_aevm_args(?sig(<<W1:256, W2:256>>)) -> {W1, W2};
format_aevm_args(?oid(<<N:256>>)) -> N;
format_aevm_args(?qid(<<N:256>>)) -> N;
format_aevm_args(<<N:256>>) -> N;
format_aevm_args({bytes, Bin}) ->
    case to_words(Bin) of
        [W] -> W;
        Ws  -> list_to_tuple(Ws)
    end;
format_aevm_args({bits, B}) -> B;
format_aevm_args(true) -> 1;
format_aevm_args(false) -> 0;
format_aevm_args([H|T]) ->
    [format_aevm_args(H) | format_aevm_args(T)];
format_aevm_args(T) when is_tuple(T) ->
    list_to_tuple(format_aevm_args(tuple_to_list(T)));
format_aevm_args(M) when is_map(M) ->
    maps:from_list(format_aevm_args(maps:to_list(M)));
format_aevm_args(X) -> X.

to_words(Bin) ->
    N      = byte_size(Bin),
    PadN   = (N + 31) div 32 * 32,
    Padded = <<Bin/binary, 0:(PadN - N)/unit:8>>,
    [ W || <<W:32/unit:8>> <= Padded ].


make_fate_function_id(FunctionName) when is_binary(FunctionName) ->
    aeb_fte_code:symbol_identifier(FunctionName).

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


create_tx(Owner, Spec0, State) ->
    Spec = maps:merge(
             #{ abi_version => aere_version:latest_sophia_abi_version()
              , vm_version  => maps:get(vm_version, Spec0, aere_version:latest_sophia_vm_version())
              , fee         => 1000000 * min_gas_price()
              , deposit     => 10
              , amount      => 200
              , gas         => 10000 }, Spec0),
    create_tx_(Owner, Spec, State).

next_nonce(PubKey, S) ->
    Account = aec_accounts_trees:get(PubKey, aec_trees:accounts(trees(S))),
    aec_accounts:nonce(Account) + 1.

priv_key(PubKey, State) ->
    maps:get(PubKey, key_pairs(State)).

calls(State) ->
    aec_trees:calls(trees(State)).


call_tx(PubKey, ContractKey, Spec0, State) ->
    Spec = maps:merge(call_tx_default_spec(PubKey, ContractKey, State), Spec0),
    {ok, Tx} = aect_call_tx:new(Spec),
    Tx.

call_tx_default_spec(PubKey, ContractKey, State) ->
    #{ fee         => 600000 * min_gas_price()
     , contract_id => aeser_id:create(contract, ContractKey)
     , caller_id   => aeser_id:create(account, PubKey)
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , abi_version => aere_version:latest_sophia_abi_version()
     , amount      => 100
     , gas         => 10000
     , gas_price   => 1 * min_gas_price()
     , call_data   => <<"CALL DATA">>
     , ttl         => 0
     }.

create_tx_(PubKey, Spec0, State) ->
    Spec = maps:merge(create_tx_default_spec(PubKey, State), Spec0),
    {ok, Tx} = aect_create_tx:new(Spec),
    Tx.

create_tx_default_spec(PubKey, State) ->
    #{ fee         => 1000000 * min_gas_price()
     , owner_id    => aeser_id:create(account, PubKey)
     , nonce       => try next_nonce(PubKey, State) catch _:_ -> 0 end
     , code        => dummy_bytecode()
     , vm_version  => aere_version:latest_sophia_vm_version()
     , abi_version => aere_version:latest_sophia_abi_version()
     , deposit     => 10
     , amount      => 200
     , gas         => 10
     , gas_price   => 1 * min_gas_price()
     , call_data   => <<"NOT ENCODED ACCORDING TO ABI">>
     , ttl         => 0
     }.

dummy_bytecode() ->
    {ok, Version} = aeso_compiler:version(),
    aect_sophia:serialize(#{byte_code => <<"NOT PROPER BYTE CODE">>,
                            type_info => [],  %% No type info
                            contract_source => "NOT PROPER SOURCE STRING",
                            compiler_version => Version,
                            payable => false},
                          aere_version:latest_sophia_contract_version()
                         ).

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

