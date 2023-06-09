-module(aere_fate).

-export([ run_contract/2
        , run_contract_debug/2
        , resume_contract_debug/2
        , add_fun_symbols_from_code/2
        , get_stack_trace/2
        , extract_fun_from_contract/3
        , extract_fun_from_bytecode/2
        , remove_contracts_from_chain/2
        ]).

-include("aere_macros.hrl").


-spec run_contract(FateCode, ReplState) -> Res | no_return()
    when FateCode  :: aeb_fate_code:fcode(),
         ReplState :: aere_repl_state:state(),
         Res       :: #{ result    := term()
                       , used_gas  := non_neg_integer()
                       , new_state := aefa_engine_state:state()
                       }.

run_contract(FateCode, RS) ->
    ES0  = setup_fate_state(FateCode, RS),
    Info = aefa_debug:set_breakpoints([], aefa_engine_state:debug_info(ES0)),
    ES1  = aefa_engine_state:set_debug_info(Info, ES0),
    case eval_state(ES1, RS) of
        {ok, Res} ->
            Res;
        {revert, #{err_msg := ErrMsg, engine_state := ES1}} ->
            StackTrace = get_stack_trace(RS, ES1),
            throw({repl_error, aere_msg:abort(ErrMsg, StackTrace)});
        {break, _} ->
            throw({internal_error, "Breakpoint found outside of debug mode"})
    end.


-spec run_contract_debug(FateCode, ReplState) -> {ok, Res} | {break, ReplState} | {revert, Err}
    when FateCode  :: aeb_fate_code:fcode(),
         ReplState :: aere_repl_state:state(),
         Res         :: #{ result    := term()
                         , used_gas  := non_neg_integer()
                         , new_state := aefa_engine_state:state()
                         },
         Err       :: #{ err_msg      := binary()
                       , engine_state := aefa_engine_state:state() }.

run_contract_debug(FateCode, RS) ->
    ES0  = setup_fate_state(FateCode, RS),
    Info = aefa_debug:set_debugger_status(continue, aefa_engine_state:debug_info(ES0)),
    ES1  = aefa_engine_state:set_debug_info(Info, ES0),
    eval_state(ES1, RS).


-spec resume_contract_debug(EngineState, ReplState) -> {ok, Res} | {break, ReplState} | {revert, Err}
    when EngineState :: aefa_engine_state:state(),
         ReplState   :: aere_repl_state:state(),
         Res         :: #{ result    := term()
                         , used_gas  := non_neg_integer()
                         , new_state := aefa_engine_state:state()
                         },
         Err       :: #{ err_msg      := binary()
                       , engine_state := aefa_engine_state:state() }.

resume_contract_debug(ES, RS) ->
    eval_state(ES, RS).


-spec eval_state(EngineState, ReplState) -> {ok, Res} | {break, ReplState} | {revert, Err}
    when EngineState :: aefa_engine_state:state(),
         ReplState   :: aere_repl_state:state(),
         Res         :: #{ result    := term()
                         , used_gas  := non_neg_integer()
                         , new_state := EngineState
                         },
         Err         :: #{ err_msg      := binary()
                         , engine_state := EngineState }.

eval_state(ES0, RS0) ->
    try
        ES1      = aefa_fate:execute(ES0),
        Res      = aefa_engine_state:accumulator(ES1),
        ChainApi = aefa_engine_state:chain_api(ES1),
        Opts     = aere_repl_state:options(RS0),
        UsedGas  = maps:get(call_gas, Opts) - aefa_engine_state:gas(ES1) - 10, %% RETURN(R) costs 10

        case aefa_debug:debugger_status(aefa_engine_state:debug_info(ES1)) of
            break ->
                RS2 = aere_repl_state:set_blockchain_state({breakpoint, ES1}, RS0),
                {break, RS2};
            _ ->
                {StateVal, _}  = aefa_fate:lookup_var({var, -1}, ES1),
                {StateType, _} = aere_repl_state:contract_state(RS0),
                ContractState  = {StateType, StateVal},
                RS1            = aere_repl_state:set_contract_state(ContractState, RS0),
                RS2            = aere_repl_state:set_blockchain_state({ready, ChainApi}, RS1),
                {ok, #{result    => Res,
                       used_gas  => UsedGas,
                       new_state => RS2}}
        end
    catch
        {aefa_fate, revert, ErrMsg, ES} ->
            {revert, #{err_msg      => ErrMsg,
                       engine_state => ES}}
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

    Breakpoints = aere_repl_state:breakpoints(RS),
    Info = aefa_debug:set_breakpoints(Breakpoints, aefa_debug:new()),

    ES0 =
        aefa_engine_state:new(
            Gas,
            Value,
            #{caller => Owner}, % Spec
            aefa_stores:put_contract_store(ContractPubkey, Store, aefa_stores:new()),
            ChainApi,
            #{ContractPubkey => {FateCode, aere_version:vm_version()}}, % Code cache
            aere_version:vm_version()
        ),
    ES1 = aefa_engine_state:update_for_remote_call(ContractPubkey, FateCode, aere_version:vm_version(), Caller, ES0),
    ES2 = aefa_fate:set_local_function(Function, false, ES1),
    ES3 = aefa_fate:bind_args([Arg || {_, _, Arg} <- Vars], ES2),
    ES4 = aefa_engine_state:set_functions(Functions, ES3),
    ES5 = aefa_fate:store_var({var, -1}, StateVal, ES4),
    ES6 = aefa_engine_state:set_debug_info(Info, ES5),
    ES6.


-spec get_stack_trace(ReplState, EngineState) -> [{Contract, FunHash, File, Line}]
    when ReplState   :: aefa_engine_state:state(),
         EngineState :: aefa_engine_state:state(),
         Contract    :: aec_keys:pubkey(),
         FunHash     :: binary(),
         File        :: string(),
         Line        :: non_neg_integer().

get_stack_trace(RS, ES0) ->
    ES       = aefa_engine_state:push_call_stack(ES0),
    Stack    = aefa_engine_state:call_stack(ES),
    Info     = aefa_engine_state:debug_info(ES),
    DbgStack = aefa_debug:call_stack(Info),
    Calls    = [ {aefa_debug:contract_name(Contract, Info), get_fun_symbol(RS, FunHash)}
                   || {_, Contract, _, FunHash, _, _, _, _, _} <- Stack ],
    [ {Contract, Symbol, File, Line}
        || {{Contract, Symbol}, {File, Line}} <- lists:zip(Calls, DbgStack) ].


-spec add_fun_symbol(ReplState, Hash, FunName) -> ReplState
    when ReplState :: aere_repl_state:state(),
         Hash      :: binary(),
         FunName   :: binary().

add_fun_symbol(RS, Hash, Name) ->
    Symbols = aere_repl_state:function_symbols(RS),
    aere_repl_state:set_function_symbols(Symbols#{Hash => Name}, RS).


-spec get_fun_symbol(ReplState, Hash) -> FunName
    when ReplState :: aere_repl_state:state(),
         Hash      :: binary(),
         FunName   :: binary().

get_fun_symbol(RS, Hash) ->
    maps:get(Hash, aere_repl_state:function_symbols(RS), Hash).


-spec add_fun_symbols(ReplState, Symbols) -> ReplState
    when ReplState :: aere_repl_state:state(),
         Symbols   :: aere_repl_state:function_symbols().

add_fun_symbols(RS0, Dict) ->
    F = fun({Hash, Name}, RS) -> add_fun_symbol(RS, Hash, Name) end,
    lists:foldl(F, RS0, maps:to_list(Dict)).


-spec add_fun_symbols_from_code(ReplState, FateCode) -> ReplState
    when FateCode  :: aeb_fate_code:fcode(),
         ReplState :: aere_repl_state:state().

add_fun_symbols_from_code(RS, Code) ->
    add_fun_symbols(RS, aeb_fate_code:symbols(Code)).


-spec extract_fun_from_contract(Pubkey, Chain, FunName) -> FateCode | no_return()
    when Pubkey   :: <<_:256>>,
         Chain    :: aefa_chain_api:state(),
         FunName  :: string(),
         FateCode :: aeb_fate_code:fcode().

extract_fun_from_contract(Pubkey, Chain, Name) ->
    case aefa_chain_api:contract_fate_bytecode(Pubkey, Chain) of
        {ok, Code, _, _} ->
            SerName = aeb_fate_code:symbol_identifier(binary:list_to_bin(Name)),
            extract_fun_from_bytecode(Code, SerName);
        error ->
            throw({repl_error, aere_msg:contract_not_found()})
    end.


-spec extract_fun_from_bytecode(FateCode, FunName) -> FateCode | no_return()
    when FateCode :: aeb_fate_code:fcode(),
         FunName  :: binary().

extract_fun_from_bytecode(Code, Name) ->
    Functions = aeb_fate_code:functions(Code),
    case maps:get(Name, Functions, not_found) of
        not_found ->
            throw({repl_error, aere_msg:function_not_found_in(Name)});
        _ ->
            Functions1 = maps:filter(fun(F, _) -> F == Name end, Functions),
            Code0 = aeb_fate_code:new(),
            Code1 = aeb_fate_code:update_symbols(Code0, aeb_fate_code:symbols(Code)),
            Code2 = aeb_fate_code:update_functions(Code1, Functions1),
            Code2
    end.


-spec remove_contracts_from_chain(Chain, Pubkeys) -> Chain
    when Chain   :: aefa_chain_api:state(),
         Pubkeys :: [<<_:256>>].

remove_contracts_from_chain(Chain, Pubkeys) ->
    lists:foldl(fun aefa_chain_api:remove_contract/2, Chain, Pubkeys).
