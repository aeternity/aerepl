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

-type stacktrace_entry() ::
        #{ contract => aefa_debug:pubkey()
         , function => binary()
         , file     => string()
         , line     => non_neg_integer()
        }.
-type stacktrace() :: list(stacktrace_entry()).

-type eval_return() ::
        #{ result    := term()
         , value     := term()
         , type      := term()
         , used_gas  := non_neg_integer()
         }.

-type revert() ::
        #{ err_msg    := binary()
         , stacktrace := stacktrace()
         }.

-type eval_result() ::
        {eval_return, eval_return()}.

-type eval_debug_result() ::
        {eval_return, eval_return()}
      | break
      | {revert, revert()}.

-export_type([ stacktrace_entry/0
             , stacktrace/0
             , eval_return/0
             , eval_result/0
             , eval_debug_result/0
             ]).

-spec run_contract(FateCode, ReplState) -> {Res, ReplState} | no_return()
    when FateCode  :: aeb_fate_code:fcode(),
         ReplState :: aere_repl_state:state(),
         Res       :: eval_return().

run_contract(FateCode, RS) ->
    ES0  = setup_fate_state(FateCode, RS),
    Info = aefa_debug:set_breakpoints([], aefa_engine_state:debug_info(ES0)),
    ES1  = aefa_engine_state:set_debug_info(Info, ES0),
    case eval_state(ES1, RS) of
        {{eval_return, Res}, RS1} ->
            { Res, RS1 };
        {{revert, #{err_msg := ErrMsg, stacktrace := StackTrace}}, _} ->
            throw({repl_fate_revert, ErrMsg, StackTrace});
        {break, _} ->
            error(breakpoint_outside_debug)
    end.

-spec run_contract_debug(FateCode, ReplState) -> {Result, ReplState} when
      FateCode  :: aeb_fate_code:fcode(),
      ReplState :: aere_repl_state:state(),
      Result :: eval_debug_result().
run_contract_debug(FateCode, RS) ->
    ES0  = setup_fate_state(FateCode, RS),
    Info = aefa_debug:set_debugger_status(continue, aefa_engine_state:debug_info(ES0)),
    ES1  = aefa_engine_state:set_debug_info(Info, ES0),
    eval_state(ES1, RS).


-spec resume_contract_debug(EngineState, ReplState) -> {Result, ReplState} when
      EngineState :: aefa_engine_state:state(),
      ReplState :: aere_repl_state:state(),
      Result :: eval_debug_result().
resume_contract_debug(ES, RS) ->
    eval_state(ES, RS).


-spec eval_state(EngineState, ReplState) -> {Result, ReplState} when
      EngineState :: aefa_engine_state:state(),
      ReplState :: aere_repl_state:state(),
      Result :: eval_debug_result().
eval_state(ES0, RS0) ->
    try
        TypeEnv  = aere_repl_state:type_env(RS0),
        Type     = aere_sophia:type_of_user_input(TypeEnv),
        ES1      = aefa_fate:execute(ES0),
        Res      = aefa_engine_state:accumulator(ES1),
        ChainApi = aefa_engine_state:chain_api(ES1),
        Opts     = aere_repl_state:options(RS0),

        %% REPL adds a RETURN(R) instruction which costs 10 gas
        ReplGasOverhead = 10,

        UsedGas  = maps:get(call_gas, Opts) - aefa_engine_state:gas(ES1) - ReplGasOverhead,

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
                { { eval_return
                  , #{ result    => format_value(Res, RS2)
                     , value     => Res
                     , used_gas  => UsedGas
                     , type      => Type}
                  }
                , RS2
                }
        end
    catch
        {aefa_fate, revert, ErrMsg, ES} ->
            RSE = aere_repl_state:set_blockchain_state({abort, ES}, RS0),
            StackTrace = get_stack_trace(RS0, ES),
            { { revert
              , #{ err_msg => ErrMsg
                 , stacktrace => StackTrace
                 }
              }
            , RSE
            }
    end.

format_value(Val, RS) ->
    #{print_format := PrintFormat} = aere_repl_state:options(RS),
    TEnv = aere_repl_state:type_env(RS),
    Type = aere_sophia:type_of_user_input(TEnv),
    format_value(PrintFormat, TEnv, Type, Val).

format_value(fate, _, _, Val) ->
    io_lib:format("~p", [Val]);
format_value(sophia, TEnv, Type, Val) ->
    aere_sophia:format_value(sophia, TEnv, Type, Val);
format_value(json, TEnv, Type, Val) ->
    aere_sophia:format_value(json, TEnv, Type, Val).

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

    Version = #{ vm => aere_version:vm_version()
               , abi => aere_version:abi_version()
               },
    Binary = aeb_fate_code:serialize(FateCode, []),
    Code = #{ byte_code => Binary
            , compiler_version => aere_version:sophia_version()
            , source_hash => <<>> %crypto:hash(sha256, OriginalSourceCode ++ [0] ++ C),
            , type_info => []
            , abi_version => aeb_fate_abi:abi_version()
            , payable => false %maps:get(payable, FCode)
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


-spec get_stack_trace(ReplState, EngineState) -> stacktrace()
    when ReplState   :: aefa_engine_state:state(),
         EngineState :: aefa_engine_state:state().

get_stack_trace(RS, ES0) ->
    ES       = aefa_engine_state:push_call_stack(ES0),
    Stack    = aefa_engine_state:call_stack(ES),
    Info     = aefa_engine_state:debug_info(ES),
    DbgStack = aefa_debug:call_stack(Info),
    Calls    = [ {aefa_debug:contract_name(Contract, Info), get_fun_symbol(RS, FunHash)}
                   || {_, Contract, _, FunHash, _, _, _, _, _} <- Stack ],
    [ #{ contract => Contract
       , function => Symbol
       , file => File
       , line => Line
       } || {{Contract, Symbol}, {File, Line}} <- lists:zip(Calls, DbgStack)
    ].


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
            throw(repl_contract_not_found)
    end.


-spec extract_fun_from_bytecode(FateCode, FunName) -> FateCode | no_return()
    when FateCode :: aeb_fate_code:fcode(),
         FunName  :: binary().

extract_fun_from_bytecode(Code, Name) ->
    Functions = aeb_fate_code:functions(Code),
    case maps:get(Name, Functions, not_found) of
        not_found ->
            throw({repl_function_not_found_in, Name});
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
