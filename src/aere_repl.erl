%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ process_input/2
        , register_modules/2, default_loaded_files/0
        ]).

-include("aere_macros.hrl").

-type repl_state() :: aere_repl_state:state().

-type fun_ref() :: {definition, aeso_syntax:qid()}
                 | {local, aeso_syntax:name()}
                 | {deployed, aeso_syntax:expr(), aeso_syntax:name()}.

%% Process an input string in the current state of the repl and respond accordingly
%% This is supposed to be called after each input to the repl
-spec process_input(repl_state(), binary() | string()) -> aere_repl_response:response().
process_input(State, String) when is_binary(String) ->
    process_input(State, binary_to_list(String));
process_input(State, String) ->
    check_wololo(String),
    try {Command, Args} = aere_parse:parse(String),
        apply_command(Command, Args, aere_repl_state:bump_nonce(State))
    of
        finish ->
            aere_repl_response:new(aere_msg:bye(), finish);
        {Out, State1} ->
            aere_repl_response:new(Out, {ok, State1});
        State1 ->
            aere_repl_response:new([], {ok, State1})
    catch
        error:E:Stacktrace ->
            aere_repl_response:new(aere_msg:internal(E, Stacktrace), internal_error);
        {repl_error, E} ->
            aere_repl_response:new(E, error);
        {revert, Err} ->
            aere_repl_response:new(aere_msg:error(Err), error);
        {aefa_fate, FateErr, _} ->
            aere_repl_response:new(aere_msg:error("FATE error: " ++ FateErr), error)
    end.

%% Easter egg, don't ask.
-spec check_wololo(string()) -> ok.
check_wololo(String) ->
    string:find(String, "wololo") =:= nomatch orelse put(wololo, wololo),
    ok.

%% Return the result of applying a repl command to the given argument
-spec apply_command(aere_parse:command(), string() | [string()], repl_state()) -> aere_repl_state:command_res().
apply_command(quit, [], _) ->
    finish;
apply_command(skip, [], State) ->
    State;
apply_command(reset, [], _) ->
    aere_repl_state:init_state();
apply_command(type, I, State) ->
    infer_type(I, State);
apply_command(state, I, State) ->
    get_ready_chain(State),
    set_state(aere_sophia:parse_body(I), State);
apply_command(eval, I, State) ->
    get_ready_chain(State),
    eval_code(I, State);
apply_command(load, Modules, State) ->
    get_ready_chain(State),
    load_modules(Modules, State);
apply_command(reload, [], State) ->
    get_ready_chain(State),
    reload_modules(State);
apply_command(set, [Option|Args], State) ->
    get_ready_chain(State),
    set_option(list_to_atom(Option), Args, State);
apply_command(help, [On], State) ->
    {aere_msg:help(On), State};
apply_command(help, _, State) ->
    {aere_msg:help(), State};
apply_command(print, [What], State) ->
    {print_state(State, What), State};
apply_command(disas, [What], State) ->
    Fate = disassemble(What, State),
    {aere_msg:output(lists:flatten(aeb_fate_asm:pp(Fate))), State};
apply_command(break, [File, Line], State) ->
    aere_debugger:add_breakpoint(File, list_to_integer(Line), State);
apply_command(delete_break, Index, State) ->
    aere_debugger:delete_breakpoint(list_to_integer(Index), State);
apply_command(ResumeKind, [], State)
  when ResumeKind == continue;
       ResumeKind == next;
       ResumeKind == step;
       ResumeKind == finish ->
    ES0 = get_breakpoint_engine_state(State),
    ES1 = aere_debugger:resume(ES0, ResumeKind),
    eval_handler(State, aere_fate:resume_contract_debug(ES1, State));
apply_command(location, [], State) ->
    ES = get_breakpoint_engine_state(State),
    {aere_msg:output(aere_debugger:source_location(ES)), State};
apply_command(print_var, [VarName], State) ->
    ES = get_breakpoint_engine_state(State),
    {aere_msg:output(aere_debugger:lookup_variable(ES, VarName)), State};
apply_command(stacktrace, [], State) ->
    ES = get_breakpoint_engine_state(State),
    {aere_msg:stacktrace(aere_fate:get_stack_trace(State, ES)), State}.

infer_type(I, State) ->
    Stmts = aere_sophia:parse_body(I),
    Contract = aere_mock:eval_contract(Stmts, State),
    {TEnv, _} = aere_sophia:typecheck(Contract, [dont_unfold, allow_higher_order_entrypoints]),
    Type = aere_sophia:type_of_user_input(TEnv),
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    TypeStrClean = re:replace(TypeStr, ?TYPE_CONTAINER ++ "[0-9]*\\.", "", [global, {return, list}]),
    {aere_msg:output(TypeStrClean), State}.

eval_code(I, State) ->
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
        _ -> error(too_much_stuff) %% FIXME
    end.

-spec set_state([aeso_syntax:stmt()], repl_state()) -> aere_repl_state:command_res().
set_state(Body, RS) ->
    Contract     = aere_mock:eval_contract(Body, RS),
    {TEnv, TAst} = aere_sophia:typecheck(Contract),
    Type         = aere_sophia:type_of_user_input(TEnv),
    ByteCode     = aere_sophia:compile_contract(TAst),
    RS1          = aere_fate:add_fun_symbols_from_code(RS, ByteCode),

    #{result := StateVal, new_state := RS2} = aere_fate:run_contract(ByteCode, RS1),

    RS3 = aere_repl_state:set_contract_state({Type, StateVal}, RS2),
    RS4 = aere_repl_state:set_vars([], RS3),
    RS5 = aere_repl_state:set_funs(#{}, RS4),
    RS5.

%%%------------------

-spec eval_expr([aeso_syntax:stmt()], repl_state()) -> aere_repl_state:command_res().
eval_expr(Body, RS) ->
    Ast          = aere_mock:eval_contract(Body, RS),
    {TEnv, TAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode     = aere_sophia:compile_contract(TAst),
    RS1          = aere_fate:add_fun_symbols_from_code(RS, ByteCode),
    RS2          = aere_repl_state:set_type_env(TEnv, RS1),
    eval_handler(RS2, aere_fate:run_contract_debug(ByteCode, RS2)).

eval_handler(_RS, {ok, #{result := Res, used_gas := Gas, new_state := RS}}) ->
    print_eval_res(RS, Res, Gas, aere_repl_state:type_env(RS));
eval_handler(RS, {revert, #{err_msg := ErrMsg, engine_state := ES}}) ->
    print_eval_stacktrace(RS, ES, ErrMsg);
eval_handler(_RS, {break, RS}) ->
    {aere_msg:output("Break"), RS}.


print_eval_stacktrace(RS, ES, ErrMsg) ->
    StackTrace = aere_fate:get_stack_trace(RS, ES),
    {aere_msg:abort(ErrMsg, StackTrace), RS}.

print_eval_res(_RS, _Res, _UsedGas, none) ->
    throw({repl_error, aere_msg:error("TypeEnv not found")});
print_eval_res(RS, Res, UsedGas, TypeEnv) ->
    #{ display_gas  := DisplayGas,
       print_unit   := PrintUnit,
       print_format := PrintFormat } = aere_repl_state:options(RS),

    Type     = aere_sophia:type_of_user_input(TypeEnv),
    PrintRes = PrintUnit orelse Res =/= {tuple, {}},
    ResStr   = format_value(PrintFormat, TypeEnv, Type, Res),
    {aere_msg:eval_result(
        if PrintRes -> ResStr; true -> none end,
        if DisplayGas -> UsedGas; true -> none end),
     aere_repl_state:set_type_env(none, RS)}.

%%%------------------

format_value(fate, _, _, Val) ->
    io_lib:format("~p", [Val]);
format_value(sophia, TEnv, Type, Val) ->
    aere_sophia:format_value(sophia, TEnv, Type, Val);
format_value(json, TEnv, Type, Val) ->
    aere_sophia:format_value(json, TEnv, Type, Val).

load_modules([], S0) ->
    S0;
load_modules(Filenames, S0) ->
    Modules = aere_utils:read_files(Filenames),
    S1 = register_modules(Modules, S0),
    S2 = register_include(element(1, lists:last(Modules)), S1),
    S2.

reload_modules(RS) ->
    LdFiles  = aere_repl_state:loaded_files(RS), 
    IncFiles = aere_repl_state:included_files(RS),
    Modules  = aere_utils:read_files(maps:keys(LdFiles)),

    RS1 = register_modules(Modules, RS),
    RS2 = lists:foldl(fun register_include/2, RS1, IncFiles),
    RS2.

-spec register_modules([{string(), binary()}], repl_state()) -> aere_repl_state:command_res().
register_modules(Modules, S0) ->
    FileMap = maps:from_list(Modules),
    [ begin
          Ast0 = aere_sophia:parse_file(File, []),
          aere_sophia:typecheck(aere_mock:ast_fillup_contract(Ast0)),
          File
      end
      || {_, File} <- Modules
    ],

    S1 = clear_context(S0),
    S2 = aere_repl_state:set_loaded_files(FileMap, S1),
    S2.

%% Removes all variables, functions, types and contracts
clear_context(S0) ->
    Contracts = [PK || {_Name, _Type, {contract, PK}} <- aere_repl_state:vars(S0)],
    Chain0 = get_ready_chain(S0),
    Chain1 = aere_fate:remove_contracts_from_chain(Chain0, Contracts),
    put(contract_code_cache, undefined),
    S1 = aere_repl_state:set_vars([], S0),
    S2 = aere_repl_state:set_funs(#{}, S1),
    S3 = aere_repl_state:set_typedefs([], S2),
    S4 = aere_repl_state:set_type_scope([], S3),
    S5 = aere_repl_state:set_included_files([], S4),
    S6 = aere_repl_state:set_included_code([], S5),
    S7 = aere_repl_state:set_blockchain_state({ready, Chain1}, S6),
    S7.

register_include(Include, S0) when is_binary(Include) ->
    register_include(binary:bin_to_list(Include), S0);
register_include(Include, S0) ->
    LdFiles  = aere_repl_state:loaded_files(S0), 
    IncCode  = aere_repl_state:included_code(S0),
    IncFiles = aere_repl_state:included_files(S0),
    case maps:get(Include, maps:merge(default_loaded_files(), LdFiles), not_loaded) of
        not_loaded ->
            throw({repl_error, aere_msg:file_not_loaded(Include)});
        File ->
            S1 = case lists:member(Include, IncFiles) of
                     true -> S0;
                     false ->
                         IncludeSet = sets:from_list(
                                        [ {FName, Code} ||
                                            {FName, Code} <- maps:to_list(LdFiles),
                                            lists:member(FName, IncFiles)
                                        ]),
                         {Ast0, _IncludeSet1} = aere_sophia:parse_file(File, IncludeSet, [keep_included, {src_file, Include}]),
                         SX = aere_repl_state:set_included_files([Include|IncFiles], S0),
                         SY = aere_repl_state:set_included_code(IncCode ++ Ast0, SX),
                         SY
                 end,
            Ast = aere_mock:eval_contract([{tuple, aere_mock:ann(), []}], S1),
            aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
            S1
    end.

%%%------------------

-spec register_letval(aeso_syntax:pat(), aeso_syntax:expr(), repl_state()) -> aere_repl_state:command_res().
register_letval(Pat, Expr, S0) ->
    NewVars = lists:filter(
                fun(Var) -> Var /= "_" end,
                aeso_syntax_utils:used_ids([aere_mock:pat_as_decl(Pat)])),
    Ast = aere_mock:letval_contract(Pat, NewVars, Expr, S0),
    {TEnv, _} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints, dont_unfold]),
    %% TODO: Try to get TypedAstUnfolded without runnning the typechecker a second time
    {_, TypedAstUnfolded} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode = aere_sophia:compile_contract(TypedAstUnfolded),

    #{result := Res, new_state := S} = aere_fate:run_contract(ByteCode, S0),

    {Vals, Types, S1} =
        case NewVars of
            [_] ->
                T = aere_sophia:type_of_user_input(TEnv),
                {[Res], [T], S};
            _ ->
                {tuple_t, _, Ts} = aere_sophia:type_of_user_input(TEnv),
                {tuple, Vs} = Res,
                {tuple_to_list(Vs), Ts, S}
        end,
    NameMap = build_fresh_name_map(ByteCode),

    Vals1 = replace_function_name(Vals, NameMap),
    Vars1 = lists:zip3(NewVars, Types, Vals1),

    OldFuns = aere_repl_state:funs(S),
    NewFuns = generated_functions(ByteCode, NameMap),

    S2 = register_vars(Vars1, S1),
    S3 = aere_repl_state:set_funs(maps:merge(OldFuns, NewFuns), S2),

    S3.

%%%------------------

-spec register_letfun(aeso_syntax:id(), [aeso_syntax:pat()], [aeso_syntax:guarded_expr()], repl_state()) -> aere_repl_state:command_res().
register_letfun(Id = {id, _, Name}, Args, Body, S0) ->
    Ast = aere_mock:letfun_contract(Id, Args, Body, S0),
    {TEnv, TypedAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode = aere_sophia:compile_contract(TypedAst),

    Type = aere_sophia:type_of_user_input(TEnv),

    NameMap = build_fresh_name_map(ByteCode),

    Funs1 = generated_functions(ByteCode, NameMap),

    FunNewName = maps:get(aeb_fate_code:symbol_identifier(binary:list_to_bin(Name)), NameMap),
    FunVal = {tuple, {FunNewName, {tuple, {}}}}, %% TODO this is broken

    S1 = register_vars([{Name, Type, FunVal}], S0),
    S2 = aere_repl_state:set_funs(maps:merge(aere_repl_state:funs(S1), Funs1), S1),

    {aere_theme:error("Warning: defining functions in REPL is WIP and may be unstable."), S2}.

register_vars(NewVars, S) ->
    OldVars = aere_repl_state:vars(S),
    Filtered = [V || V = {Id, _, _} <- OldVars, [] =:= proplists:lookup_all(Id, NewVars)],
    aere_repl_state:set_vars(NewVars ++ Filtered, S).

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

-spec register_typedef(aeso_syntax:id(), [aeso_syntax:tvar()], aeso_syntax:typedef(), repl_state()) -> aere_repl_state:command_res().
register_typedef({id, _, Name}, Args, Def, S0) ->
    case Name of
        "state" -> throw({repl_error, aere_msg:state_typedef()});
        "event" -> throw({repl_error, aere_msg:event_typedef()});
        _       -> ok
    end,

    NamespaceName = ?TYPE_CONTAINER(aere_repl_state:query_nonce(S0)),

    TypeScope = aere_repl_state:type_scope(S0),
    TypeScope1 = proplists:delete(Name, TypeScope),
    S1 = aere_repl_state:set_type_scope(TypeScope1, S0),

    Def1 = unfold_types_in_type(Def, S1),

    % Check if definition is valid
    Ast = aere_mock:typedef_contract(Name, Args, Def1, S1),
    aere_sophia:typecheck(Ast),

    TypeDefEntry = {NamespaceName, Name, Args, Def1},
    TypeScope2 = [{Name, {NamespaceName, length(Args)}}|TypeScope1],

    TypeDefs  = aere_repl_state:typedefs(S0),

    S2 = aere_repl_state:set_typedefs([TypeDefEntry|TypeDefs], S1),
    S3 = aere_repl_state:set_type_scope(TypeScope2, S2),

    S3.

unfold_types_in_type(T, S0) ->
    Ast = aere_mock:type_unfold_contract(S0),
    {TEnv, _} = aere_sophia:typecheck(Ast, [dont_unfold]),
    TEnv1 = aeso_ast_infer_types:switch_scope([?MOCK_CONTRACT], TEnv),
    T1 = aeso_ast_infer_types:unfold_types_in_type(TEnv1, T),
    T1.

-spec disassemble(string(), repl_state()) -> term(). %% -> bytecode
disassemble(What, S0) ->
    Chain0 = aere_repl_state:chain_api(S0),
    S1 = aere_repl_state:set_blockchain_state({ready, Chain0}, S0),
    case parse_fun_ref(What) of
        {deployed, Expr, Name} ->
            Contract = aere_mock:eval_contract(Expr, S1),
            {_, TAst} = aere_sophia:typecheck(Contract),
            MockByteCode = aere_sophia:compile_contract(TAst),
            #{result := {contract, Pubkey}, new_state := S2} = aere_fate:run_contract(MockByteCode, S1),
            Chain = aere_repl_state:chain_api(S2),
            aere_fate:extract_fun_from_contract(Pubkey, Chain, Name);
        {definition, Id} ->
            Contract = aere_mock:eval_contract(Id, S1),
            {_, TAst} = aere_sophia:typecheck(Contract, [allow_higher_order_entrypoints]),
            MockByteCode = aere_sophia:compile_contract(TAst),
            #{result := {tuple, {FName, _}}} = aere_fate:run_contract(MockByteCode, S1),
            aere_fate:extract_fun_from_bytecode(MockByteCode, FName);
        {local, _Name} ->
            throw(not_supported)
    end.

-spec parse_fun_ref(string()) -> fun_ref().
parse_fun_ref(What) ->
    What1 = aere_sophia:parse_body(What),
    case What1 of
        [{qid, _, _} = Qid] -> {definition, Qid};
        [{id, _, Name}] -> {local, Name};
        [{proj, _, Contr, {id, _, Name}}] -> {deployed, Contr, Name};
        _ -> throw({repl_error, aere_msg:bad_fun_ref()})
    end.

-spec default_loaded_files() -> #{string() => binary()}.
default_loaded_files() ->
    case get(aere_default_loaded_files) of
        undefined ->
            StdlibDir = aeso_stdlib:stdlib_include_path(),
            Files =
                [ File ||
                    File <- element(2, file:list_dir(StdlibDir)),
                    filename:extension(File) =:= ".aes"
                ],
            FileMap = maps:from_list(aere_utils:read_files(Files)),
            put(aere_default_loaded_files, FileMap),
            FileMap;
        Files ->
            Files
    end.

get_ready_chain(RS) ->
    case aere_repl_state:blockchain_state(RS) of
        {ready, Chain} ->
            Chain;
        _ ->
            throw({repl_error, aere_msg:chain_not_ready()})
    end.

get_breakpoint_engine_state(RS) ->
    case aere_repl_state:blockchain_state(RS) of
        {breakpoint, ES} -> ES;
        _                -> throw({repl_error, aere_msg:not_at_breakpoint()})
    end.

set_option(Option, Args, RS) ->
    Opts = aere_repl_state:options(RS),
    Locked = maps:get(locked_opts, Opts, []),
    LockedOption = lists:member(Option, [locked_opts|Locked]),
    LockedOption andalso throw({repl_error, aere_msg:locked_option()}),
    case aere_options:parse_option(Option, Args) of
        error ->
            {aere_msg:option_usage(Option), RS};
        Val ->
            NewOpts = Opts#{Option => Val},
            aere_repl_state:set_options(NewOpts, RS)
    end.

print_state(RS, What) ->
    Vars  = aere_repl_state:vars(RS),
    Types = aere_repl_state:typedefs(RS),
    Opts  = aere_repl_state:options(RS),
    Files = aere_repl_state:loaded_files(RS),
    Incs  = aere_repl_state:included_files(RS),
    BPs   = aere_repl_state:breakpoints(RS),
    PrintFuns =
        #{ "vars"        => {fun aere_msg:list_vars/1, Vars},
           %% "funs"        => {fun aere_msg:list_funs/1, Funs},
           "types"       => {fun aere_msg:list_types/1, Types},
           "options"     => {fun aere_msg:list_options/1, Opts},
           "files"       => {fun aere_msg:list_loaded_files/1, Files},
           "includes"    => {fun aere_msg:list_includes/1, Incs},
           "breakpoints" => {fun aere_msg:list_breakpoints/1, BPs}
        },
    case maps:get(What, PrintFuns, unknown) of
        unknown ->
            throw({repl_error, aere_msg:list_unknown(maps:keys(PrintFuns))});
        {Print, Payload} ->
            Print(Payload)
    end.
