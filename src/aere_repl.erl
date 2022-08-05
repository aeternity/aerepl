%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ init_state/0
        , process_input/2
        ]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

-spec init_options() -> repl_options().
init_options() ->
    #{ theme => aere_theme:default_theme()
    , display_gas => false
    , call_gas => 100000000000000000
    , call_value => 0
    , print_format => sophia
    , print_unit => false
    }.

-spec init_state() -> repl_state().
init_state() ->
    Trees0 = aec_trees:new(),
    {PK, Trees} = aere_chain:new_account(100000000000000000000000000000, Trees0),
    ChainState = aefa_chain_api:new(
                   #{ gas_price => 1,
                      fee       => 0,
                      trees     => Trees,
                      origin    => PK,
                      tx_env    => aere_chain:default_tx_env(1)
                   }
                  ),
    S0 = #repl_state{
       blockchain_state = {ready, ChainState},
       repl_account     = PK,
       options          = init_options(),
       contract_state   = ?DEFAULT_CONTRACT_STATE
      },
    S1 = add_modules(default_loaded_files(), S0),
    S1.

%% Process an input string in the current state of the repl and respond accordingly
%% This is supposed to be called after each input to the repl
-spec process_input(repl_state(), binary() | string()) -> repl_response().
process_input(State, String) when is_binary(String) ->
    process_input(State, binary_to_list(String));
process_input(State, String) ->
    check_wololo(String),
    try {Command, Args} = aere_parse:parse(String),
         apply_command(Command, Args, bump_nonce(State))
    of
        {Out, State1 = #repl_state{}} ->
            #repl_response
                { output = Out
                , warnings = []
                , status = {ok, State1}
                };
        State1 = #repl_state{} ->
            #repl_response
                { output = []
                , warnings = []
                , status = {ok, State1}
                };
        finish ->
            #repl_response
                { output = aere_msg:bye()
                , warnings = []
                , status = finish
                }
    catch error:E:Stacktrace ->
            #repl_response
                { output = aere_msg:internal(E, Stacktrace)
                , warnings = []
                , status = internal_error
                };
          {repl_error, E} ->
            #repl_response
                { output = E
                , warnings = []
                , status = error
                };
          {revert, Err} ->
            #repl_response
                { output = aere_msg:abort(Err)
                , warnings = []
                , status = error
                };
          {aefa_fate, FateErr, _} ->
            #repl_response
                { output = aere_msg:error("FATE error: " ++ FateErr)
                , warnings = []
                , status = error
                }
    end.

%% Easter egg, don't ask.
-spec check_wololo(string()) -> ok.
check_wololo(String) ->
    string:find(String, "wololo") =:= nomatch orelse put(wololo, wololo),
    ok.

%% Return the result of applying a repl command to the given argument
-spec apply_command(aere_parse:command(), string() | [string()], repl_state()) -> command_res().
apply_command(quit, [], _) ->
    finish;
apply_command(reset, [], _) ->
    init_state();
apply_command(type, I, State) ->
    Stmts = aere_sophia:parse_body(I),
    Contract = aere_mock:eval_contract(Stmts, State),
    {TEnv, _} = aere_sophia:typecheck(Contract, [dont_unfold, allow_higher_order_entrypoints]),
    Type = aere_sophia:type_of_user_input(TEnv),
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    TypeStrClean = re:replace(TypeStr, ?TYPE_CONTAINER ++ "[0-9]*\\.", "", [global, {return, list}]),
    {aere_msg:output(TypeStrClean), State};
apply_command(state, I, State) ->
    set_state(aere_sophia:parse_body(I), State);
apply_command(eval, I, State) ->
    Parse = aere_sophia:parse_top(I),
    case Parse of
        {body, Body} ->
            eval_expr(Body, State);
        [{include, _, {string, _, Inc}}] ->
            register_include(Inc, State);
        [{letval, _, Pat, Expr}] ->
            register_letval(Pat, Expr, State);
        [{letfun, _, FName, Args, _, Body}] ->
            register_letfun(FName, Args, Body, State);
        [{type_def, _, Name, Args, Body}] ->
            register_typedef(Name, Args, Body, State);
        _ -> error(too_many_shit)
    end;
apply_command(load, Modules, State) ->
    load_modules(Modules, State);
apply_command(reload, Modules, State) ->
    reload_modules(Modules, State);
apply_command(add, Modules, State) ->
    add_modules(Modules, State);
apply_command(module, Modules, State) ->
    register_include(Modules, State);
apply_command(set, [Option|Args], State) ->
    set_option(list_to_atom(Option), Args, State);
apply_command(help, Arg, State) ->
    case Arg of
        [On] -> {aere_msg:help(On), State};
        _ -> {aere_msg:help(), State}
    end;
apply_command(print, [What], State) ->
    {print_state(State, What), State};
apply_command(continue, _, State = #repl_state{blockchain_state = BS}) ->
    case BS of
        {ready, _} ->
            {aere_msg:error("Not at breakpoint!"), State};
        {breakpoint, ES} ->
            Stack = aefa_engine_state:accumulator_stack(ES),
            StackS = io_lib:format("~p", [Stack]),
            {StackS, State}
    end.

-spec set_state([aeso_syntax:stmt()], repl_state()) -> command_res().
set_state(Body, S0) ->
    Contract = aere_mock:eval_contract(Body, S0),
    {TEnv, TAst} = aere_sophia:typecheck(Contract),
    Type = aere_sophia:type_of_user_input(TEnv),
    ByteCode = aere_sophia:compile_contract(TAst),
    {StateVal, _, S1} = run_contract(ByteCode, S0),

    S2 = S1#repl_state{contract_state = {Type, StateVal},
                       vars = [],
                       funs = #{}
                      },
    S2.

-spec eval_expr([aeso_syntax:stmt()], repl_state()) -> command_res().
eval_expr(Body, S0 = #repl_state{options = #{display_gas  := DisplayGas,
                                             print_unit   := PrintUnit,
                                             print_format := PrintFormat
                                           }}) ->
    Ast = aere_mock:eval_contract(Body, S0),
    {TEnv, TAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode = aere_sophia:compile_contract(TAst),
    {Res, UsedGas, S1} = run_contract(ByteCode, S0),
    case {PrintUnit, Res, DisplayGas} of
        {false, {tuple, {}}, false} -> S1;
        {false, {tuple, {}}, true} -> {aere_msg:used_gas(UsedGas), S1};
        _ ->
            Type = aere_sophia:type_of_user_input(TEnv),
            ResStr = aere_msg:output(format_value(PrintFormat, TEnv, Type, Res)),
	    GasStr = [aere_msg:used_gas(UsedGas) || DisplayGas],
            {[ResStr|GasStr], S1}
    end.

format_value(fate, _, _, Val) ->
    io_lib:format("~p", [Val]);
format_value(sophia, TEnv, Type, Val) ->
    aere_sophia:format_value(sophia, TEnv, Type, Val);
format_value(json, TEnv, Type, Val) ->
    aere_sophia:format_value(json, TEnv, Type, Val).

load_modules(Modules, S0) ->
    S1 = S0#repl_state{loaded_files = #{}},
    S2 = add_modules(default_loaded_files() ++ Modules, S1),
    register_include(lists:last(Modules), S2).

reload_modules([], S0 = #repl_state{loaded_files = LdFiles}) ->
    reload_modules(maps:keys(LdFiles), S0);
reload_modules(Modules, S0 = #repl_state{included_files = IncFiles}) ->
    S1 = add_modules(Modules -- default_loaded_files(), S0),
    S2 = lists:foldl(fun register_include/2, S1, IncFiles),
    S2.

add_modules([], S0) ->
    S0; %% Can happen
add_modules(Modules, S0 = #repl_state{loaded_files = LdFiles}) ->
    Files = read_modules(Modules),

    S1 = clear_context(S0),
    S2 = S1#repl_state{loaded_files = maps:merge(LdFiles, maps:from_list(lists:zip(Modules, Files)))},
    S2.

% Reads files either from working dir or stdlib
read_file(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            file:read_file(filename:join(aeso_stdlib:stdlib_include_path(), Filename));
        Res -> Res
    end.

read_modules(Modules) ->
    Files = [read_file(M) || M <- Modules],

    case [{File, file:format_error(Err)} || {File, {error, Err}} <- lists:zip(Modules, Files)] of
        []     -> ok;
        Failed -> throw({repl_error, aere_msg:files_load_error(Failed)})
    end,

    OkFiles =
        [ begin
              Ast0 = aeso_parser:string(binary:bin_to_list(File)),
              aere_sophia:typecheck(aere_mock:ast_fillup_contract(Ast0)),
              File
          end
          || {ok, File} <- Files
        ],

    OkFiles.

%% Removes all variables, functions, types and contracts
clear_context(S0 = #repl_state{vars = Vars}) ->
    Contracts = [PK || {_Name, _Type, {contract, PK}} <- Vars],
    Chain0 = get_ready_chain(S0),
    Chain1 = lists:foldl(fun aefa_chain_api:remove_contract/2, Chain0, Contracts),
    put(contract_code_cache, undefined),
    S0#repl_state{
      vars = [],
      funs = #{},
      typedefs = [],
      type_scope = [],
      included_files = [],
      included_code = [],
      blockchain_state = {ready, Chain1}}.

register_include(Include, S0) when is_binary(Include) ->
    register_include(binary:bin_to_list(Include), S0);
register_include(Include, S0 = #repl_state{included_files = IncFiles, included_code = IncCode, loaded_files = LdFiles}) ->
    case maps:get(Include, LdFiles, not_loaded) of
        not_loaded ->
            throw({repl_error, aere_msg:file_not_loaded(Include)});
        File ->
            S1 = case lists:member(Include, IncFiles) of
                     true -> S0;
                     false ->
                         IncludeSet = sets:from_list([{FName, Code} || {FName, Code} <- maps:to_list(LdFiles), lists:member(FName, IncFiles)]),
                         {Ast0, _IncludeSet1} = aere_sophia:parse_file(File, IncludeSet, [keep_included]),
                         S0#repl_state{included_files = [Include|IncFiles], included_code = IncCode ++ Ast0}
                 end,
            Ast = aere_mock:eval_contract([{tuple, aere_mock:ann(), []}], S1),
            aere_sophia:typecheck(Ast),
            S1
    end.

-spec register_letval(aeso_syntax:pat(), aeso_syntax:expr(), repl_state()) -> command_res().
register_letval(Pat, Expr, S0 = #repl_state{funs = Funs}) ->
    NewVars = lists:filter(
                fun(Var) -> Var /= "_" end,
                aeso_syntax_utils:used_ids([aere_mock:pat_as_decl(Pat)])),
    Ast = aere_mock:letval_contract(Pat, NewVars, Expr, S0),
    {TEnv, TypedAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode = aere_sophia:compile_contract(TypedAst),

    {Vals, Types, S1} =
        case NewVars of
            [_] ->
                T = aere_sophia:type_of_user_input(TEnv),
                {V, _, S} = run_contract(ByteCode, S0),
                {[V], [T], S};
            _ ->
                {tuple_t, _, Ts} = aere_sophia:type_of_user_input(TEnv),
                {{tuple, Vs}, _, S} = run_contract(ByteCode, S0),
                {tuple_to_list(Vs), Ts, S}
        end,
    NameMap = build_fresh_name_map(ByteCode),

    Vals1 = replace_function_name(Vals, NameMap),
    Vars1 = lists:zip3(NewVars, Types, Vals1),
    Funs1 = generated_functions(ByteCode, NameMap),

    S2 = register_vars(Vars1, S1),

    S2#repl_state{funs = maps:merge(Funs, Funs1)}.

-spec register_letfun(aeso_syntax:id(), [aeso_syntax:pat()], [aeso_syntax:guarded_expr()], repl_state()) -> command_res().
register_letfun(Id = {id, _, Name}, Args, Body, S0 = #repl_state{funs = Funs}) ->
    Ast = aere_mock:letfun_contract(Id, Args, Body, S0),
    {TEnv, TypedAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode = aere_sophia:compile_contract(TypedAst),

    Type = aere_sophia:type_of_user_input(TEnv),

    NameMap = build_fresh_name_map(ByteCode),

    Funs1 = generated_functions(ByteCode, NameMap),

    FunNewName = maps:get(aeb_fate_code:symbol_identifier(binary:list_to_bin(Name)), NameMap),
    FunVal = {tuple, {FunNewName, {tuple, {}}}}, %% TODO this is broken

    S1 = register_vars([{Name, Type, FunVal}], S0),

    {aere_theme:error("Warning: defining functions in REPL is WIP and may be unstable."), S1#repl_state{funs = maps:merge(Funs, Funs1)}}.

register_vars(NewVars, S = #repl_state{vars = OldVars}) ->
    Filtered = [V || V = {Id, _, _} <- OldVars, [] =:= proplists:lookup_all(Id, NewVars)],
    S#repl_state{vars = NewVars ++ Filtered}.

build_fresh_name_map(ByteCode) ->
    Symbols = aeb_fate_code:symbols(ByteCode),
    AddedFuns = [FSym || {FSym, FName} <- maps:to_list(Symbols), not ?IS_REPL_ENTRYPOINT(FName)],
    NewAddedFuns = [binary:list_to_bin(erlang:ref_to_list(make_ref()))|| _ <- AddedFuns],
    maps:from_list(lists:zip(AddedFuns, NewAddedFuns)).

generated_functions(ByteCode, NameMap) ->
    Funs = maps:to_list(aeb_fate_code:functions(ByteCode)),
    Filter = fun({FName, Def}) ->
                     not ?IS_REPL_ENTRYPOINT(FName) andalso
                         {true, {maps:get(FName, NameMap, FName), replace_function_name(Def, NameMap)}}
             end,
    maps:from_list(lists:filtermap(Filter, Funs)).

replace_function_name({tuple, {FName, Closure}}, NameMap) when is_binary(FName) ->
    {tuple, {maps:get(FName, NameMap, FName), Closure}};
replace_function_name(E = {I, {immediate, FName}}, NameMap) when is_atom(I) andalso is_binary(FName) ->
    case atom_to_list(I) of
        "CALL" ++ _ -> {I, {immediate, maps:get(FName, NameMap, FName)}};
        _ -> E
    end;
replace_function_name(T, NameMap) when is_tuple(T) ->
    list_to_tuple(replace_function_name(tuple_to_list(T), NameMap));
replace_function_name([H|T], NameMap) ->
    [replace_function_name(H, NameMap)|replace_function_name(T, NameMap)];
replace_function_name(M, NameMap) when is_map(M) ->
    maps:from_list(replace_function_name(maps:to_list(M), NameMap));
replace_function_name(E, _) ->
    E.

-spec register_typedef(aeso_syntax:id(), [aeso_syntax:tvar()], aeso_syntax:typedef(), repl_state()) -> command_res().
register_typedef({id, _, Name}, Args, Def, S0 = #repl_state{query_nonce = Nonce, typedefs = TypeDefs, type_scope = TypeScope}) ->
    NamespaceName = ?TYPE_CONTAINER(Nonce),

    TypeScope1 = proplists:delete(Name, TypeScope),
    S1 = S0#repl_state{type_scope = TypeScope1},

    Def1 = unfold_types_in_type(Def, S1),

    % Check if definition is valid
    Ast = aere_mock:typedef_contract(Name, Args, Def1, S1),
    aere_sophia:typecheck(Ast),

    TypeDefEntry = {NamespaceName, Name, Args, Def1},
    TypeScope2 = [{Name, {NamespaceName, length(Args)}}|TypeScope1],

    S1#repl_state{typedefs = [TypeDefEntry|TypeDefs], type_scope = TypeScope2}.

unfold_types_in_type(T, S0) ->
    Ast = aere_mock:type_unfold_contract(S0),
    {TEnv, _} = aere_sophia:typecheck(Ast, [dont_unfold]),
    TEnv1 = aeso_ast_infer_types:switch_scope([?MOCK_CONTRACT], TEnv),
    T1 = aeso_ast_infer_types:unfold_types_in_type(TEnv1, T),
    T1.

run_contract(ByteCode, S) ->
    ES = setup_fate_state(ByteCode, S),
    eval_state(ES, S).

eval_state(ES0, S = #repl_state{contract_state = {StateType, _}}) ->
    try ES1 = aefa_fate:execute(ES0),
         {StateVal, ES2} = aefa_fate:lookup_var({var, -1}, ES1),

         Res       = aefa_engine_state:accumulator(ES2),
         ChainApi  = aefa_engine_state:chain_api(ES2),
         UsedGas   = maps:get(call_gas, S#repl_state.options) - aefa_engine_state:gas(ES2) - 10, %% RETURN(R) costs 10

         {Res, UsedGas, S#repl_state{blockchain_state = {ready, ChainApi},
                                     contract_state = {StateType, StateVal}
                                    }}
    catch {aefa_fate, revert, ErrMsg, _} ->
            throw({revert, ErrMsg})
    end.

setup_fate_state(
  ByteCode,
  #repl_state{
     repl_account = Owner,
     blockchain_state = {ready, ChainApi0},
     vars = Vars,
     funs = Funs,
     options = #{call_gas := Gas, call_value := Value},
     contract_state = {_, StateVal}
    }) ->

    Version = #{ vm => aere_version:vm_version(), abi => aere_version:abi_version() },
    Binary = aeb_fate_code:serialize(ByteCode, []),
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

    setup_fate_state(aect_contracts:pubkey(Contract), ByteCode, Owner, Caller, Function, Vars, Gas, Value, StateVal, Funs, ChainApi).

setup_fate_state(Contract, ByteCode, Owner, Caller, Function, Vars, Gas, Value, StateVal, Functions0, ChainApi) ->
    Store = aefa_stores:initial_contract_store(),
    Functions = maps:merge(Functions0, aeb_fate_code:functions(ByteCode)),
    ES0 =
        aefa_engine_state:new(
          Gas,
          Value,
          #{caller => Owner}, % Spec
          aefa_stores:put_contract_store(Contract, Store, aefa_stores:new()),
          ChainApi,
          #{Contract => {ByteCode, aere_version:vm_version()}}, % Code cache
          aere_version:vm_version()
         ),
    ES1 = aefa_engine_state:update_for_remote_call(Contract, ByteCode, aere_version:vm_version(), Caller, ES0),
    ES2 = aefa_fate:set_local_function(Function, false, ES1),
    ES3 = aefa_fate:bind_args([Arg || {_, _, Arg} <- Vars], ES2),
    ES4 = aefa_engine_state:set_functions(Functions, ES3),
    ES5 = aefa_fate:store_var({var, -1}, StateVal, ES4),

    ES5.

bump_nonce(S = #repl_state{query_nonce = N}) ->
    S#repl_state{query_nonce = N + 1}.

default_loaded_files() ->
    StdlibDir = aeso_stdlib:stdlib_include_path(),
    Files =
        [ File ||
            File <- element(2, file:list_dir(StdlibDir)),
            filename:extension(File) =:= ".aes"
        ],
    Files.

get_ready_chain(#repl_state{blockchain_state = {ready, Chain}}) ->
    Chain;
get_ready_chain(_) ->
    throw({repl_error, aere_msg:chain_not_ready()}).

set_option(Option, Args, S = #repl_state{options = Opts}) ->
    case aere_options:parse_option(Option, Args) of
        error ->
            {aere_msg:option_usage(Option), S};
        Val -> S#repl_state{options = Opts#{Option => Val}}
    end.

print_state(#repl_state{
               vars = Vars,
               %% funs = Funs,
               typedefs = Types,
               options = Opts,
               loaded_files = Files,
               included_files = Incs
            }, What) ->
    PrintFuns =
        #{ "vars" => {fun aere_msg:list_vars/1, Vars},
           %% "funs" => {fun aere_msg:list_funs/1, Funs},
           "types" => {fun aere_msg:list_types/1, Types},
           "options" => {fun aere_msg:list_options/1, Opts},
           "files" => {fun aere_msg:list_loaded_files/1, Files},
           "includes" => {fun aere_msg:list_includes/1, Incs}
        },
    case maps:get(What, PrintFuns, unknown) of
        unknown ->
            throw({repl_error, aere_msg:list_unknown(maps:keys(PrintFuns))});
        {Print, Payload} -> Print(Payload)
    end.
