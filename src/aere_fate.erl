-module(aere_fate).

-export([ run_contract/2
        , run_contract_debug/2
        , eval_state/2
        , add_fun_symbols_from_code/1
        , get_stack_trace/1
        ]).

-include("aere_macros.hrl").


-define(AEREPL_FUN_SYMBOLS_ETS, aerepl_fun_smbols).


-spec run_contract(FateCode, ReplState) -> {Message, ReplState} | no_return()
    when FateCode  :: aeb_fate_code:fcode(),
         ReplState :: aere_repl_state:state(),
         Message   :: aere_theme:renderable().

run_contract(FateCode, S) ->
    ES = setup_fate_state(FateCode, S),
    case eval_state(ES, S) of
        {revert, #{err_msg := ErrMsg, engine_state := ES1}} ->
            StackTrace = get_stack_trace(ES1),
            throw({repl_error, aere_msg:abort(ErrMsg, StackTrace)});
        Res ->
            Res
    end.


-spec run_contract_debug(FateCode, ReplState) -> {Message, ReplState} | {revert, Err}
    when FateCode  :: aeb_fate_code:fcode(),
         ReplState :: aere_repl_state:state(),
         Message   :: aere_theme:renderable(),
         Err       :: #{ err_msg      := binary()
                       , engine_state := aefa_engine_state:state() }.

run_contract_debug(FateCode, S) ->
    ES = setup_fate_state(FateCode, S),
    case eval_state(ES, S) of
        {revert, Err} ->
            Callback = aere_repl_state:callback(S),
            Callback({S, Err});
        Res ->
            Res
    end.


-spec eval_state(EngineState, ReplState) -> {Message, ReplState} | {revert, Err}
    when EngineState :: aefa_engine_state:state(),
         ReplState   :: aere_repl_state:state(),
         Message     :: aere_theme:renderable(),
         Err         :: #{ err_msg      := binary()
                         , engine_state := EngineState }.

eval_state(ES0, S) ->
    try
        ES1             = aefa_fate:execute(ES0),
        {StateVal, ES2} = aefa_fate:lookup_var({var, -1}, ES1),

        Res      = aefa_engine_state:accumulator(ES2),
        ChainApi = aefa_engine_state:chain_api(ES2),
        Opts     = aere_repl_state:options(S),
        UsedGas  = maps:get(call_gas, Opts) - aefa_engine_state:gas(ES2) - 10, %% RETURN(R) costs 10

        NewBlockchainState = case aefa_engine_state:breakpoint_stop(ES2) of
                                 true  -> {breakpoint, ES2};
                                 false -> {running, ChainApi, Res, UsedGas}
                             end,
        {StateType, _} = aere_repl_state:contract_state(S),
        NewContractState = {StateType, StateVal},

        NewReplState0 = aere_repl_state:set_blockchain_state(NewBlockchainState, S),
        NewReplState = aere_repl_state:set_contract_state(NewContractState, NewReplState0),

        case NewBlockchainState of
            {breakpoint, _} ->
                {aere_msg:output("Break"), NewReplState};
            {running, _, _, _} ->
                Callback = aere_repl_state:callback(S),
                Callback(NewReplState)
        end
    catch {aefa_fate, revert, ErrMsg, ES} ->
            {revert, #{err_msg => ErrMsg,
                       engine_state => ES
                      }}
    end.


-spec setup_fate_state(FateCode, ReplState) -> EngineState
    when FateCode    :: aeb_fate_code:fcode(),
         ReplState   :: aere_repl_state:state(),
         EngineState :: aefa_engine_state:state().

setup_fate_state(FateCode, RS) ->
    Owner = aere_repl_state:repl_account(RS),
    {ready, ChainApi0} = aere_repl_state:blockchain_state(RS),
    Vars = aere_repl_state:vars(RS),
    Funs = aere_repl_state:funs(RS),
    #{call_gas := Gas, call_value := Value} = aere_repl_state:options(RS),
    {_, StateVal} = aere_repl_state:contract_state(RS),
    Breakpoints = aere_repl_state:breakpoints(RS),

    Version = #{ vm => aere_version:vm_version(), abi => aere_version:abi_version() },
    Binary = aeb_fate_code:serialize(FateCode, []),
    Code = #{byte_code => Binary,
             compiler_version => aere_version:sophia_version(),
             source_hash => <<>>,%crypto:hash(sha256, OriginalSourceCode ++ [0] ++ C),
             type_info => [],
             abi_version => aeb_fate_abi:abi_version(),
             payable => false %maps:get(payable, FCode)
            },
    Contract = aect_contracts:new(Owner, 0, Version, aeser_contract_code:serialize(Code), 0),
    ChainApi = aefa_chain_api:put_contract(Contract, ChainApi0),
    Function = aeb_fate_code:symbol_identifier(<<?USER_INPUT>>),

    Caller = aeb_fate_data:make_address(Owner),

    ContractPubkey = aect_contracts:pubkey(Contract),

    Store = aefa_stores:initial_contract_store(),
    Functions = maps:merge(Funs, aeb_fate_code:functions(FateCode)),

    ES0 =
        aefa_engine_state:new_dbg(
            Gas,
            Value,
            #{caller => Owner}, % Spec
            aefa_stores:put_contract_store(ContractPubkey, Store, aefa_stores:new()),
            ChainApi,
            #{ContractPubkey => {FateCode, aere_version:vm_version()}}, % Code cache
            aere_version:vm_version(),
            Breakpoints
        ),
    ES1 = aefa_engine_state:update_for_remote_call(ContractPubkey, FateCode, aere_version:vm_version(), Caller, ES0),
    ES2 = aefa_fate:set_local_function(Function, false, ES1),
    ES3 = aefa_fate:bind_args([Arg || {_, _, Arg} <- Vars], ES2),
    ES4 = aefa_engine_state:set_functions(Functions, ES3),
    ES5 = aefa_fate:store_var({var, -1}, StateVal, ES4),
    ES5.


-spec get_stack_trace(EngineState) -> [{Contract, FunHash, BasicBlock}]
    when EngineState :: aefa_engine_state:state(),
         Contract    :: aec_keys:pubkey(),
         FunHash     :: binary(),
         BasicBlock  :: non_neg_integer().

get_stack_trace(ES) ->
    Stack = aefa_engine_state:call_stack(aefa_engine_state:push_call_stack(ES)),
    [ {Contract, get_fun_symbol(FunHash), BB}
      || {_, Contract, _, FunHash, _, BB, _, _, _} <- Stack
    ].


-spec ensure_fun_symbols() -> true | ets:table().

ensure_fun_symbols() ->
    ets:whereis(?AEREPL_FUN_SYMBOLS_ETS) =/= undefined
        orelse ets:new(?AEREPL_FUN_SYMBOLS_ETS, [named_table, set, public]).


-spec add_fun_symbol(Hash, FunName) -> true
    when Hash    :: binary(),
         FunName :: binary().

add_fun_symbol(Hash, Name) ->
    ensure_fun_symbols(),
    ets:insert(?AEREPL_FUN_SYMBOLS_ETS, {Hash, Name}).


-spec get_fun_symbol(Hash) -> FunName | Hash
    when Hash    :: binary(),
         FunName :: binary().

get_fun_symbol(Hash) ->
    ensure_fun_symbols(),
    case ets:lookup(?AEREPL_FUN_SYMBOLS_ETS, Hash) of
        [{_, Name}|_] -> Name;
        [] -> Hash
    end.


-spec add_fun_symbols(Symbols) -> ok
    when Symbols :: map().

add_fun_symbols(Dict) ->
    [ add_fun_symbol(Hash, Name) || {Hash, Name} <- maps:to_list(Dict) ],
    ok.


-spec add_fun_symbols_from_code(FateCode) -> ok
    when FateCode :: aeb_fate_code:fcode().

add_fun_symbols_from_code(Code) ->
    add_fun_symbols(aeb_fate_code:symbols(Code)).
