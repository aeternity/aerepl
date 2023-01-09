%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ process_input/2
        , register_modules/2, default_loaded_files/0
        ]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

-type repl_state() :: aere_repl_state:state().

-type fun_ref() :: {definition, aeso_syntax:qid()}
                 | {local, aeso_syntax:name()}
                 | {deployed, aeso_syntax:expr(), aeso_syntax:name()}.

%% Process an input string in the current state of the repl and respond accordingly
%% This is supposed to be called after each input to the repl
-spec process_input(repl_state(), binary() | string()) -> repl_response().
process_input(State, String) when is_binary(String) ->
    process_input(State, binary_to_list(String));
process_input(State, String) ->
    check_wololo(String),
    try {Command, Args} = aere_parse:parse(String),
        apply_command(Command, Args, aere_repl_state:bump_nonce(State))
    of
        finish ->
            #repl_response
                { output = aere_msg:bye()
                , warnings = []
                , status = finish
                };
        {Out, State1} ->
            #repl_response
                { output = Out
                , warnings = []
                , status = {ok, State1}
                };
        State1 ->
            #repl_response
                { output = []
                , warnings = []
                , status = {ok, State1}
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
-spec apply_command(aere_parse:command(), string() | [string()], repl_state()) -> aere_repl_state:command_res().
apply_command(quit, [], _) ->
    finish;
apply_command(skip, [], State) ->
    State;
apply_command(reset, [], _) ->
    aere_repl_state:init_state();
apply_command(type, I, State) ->
    Stmts = aere_sophia:parse_body(I),
    Contract = aere_mock:eval_contract(Stmts, State),
    {TEnv, _} = aere_sophia:typecheck(Contract, [dont_unfold, allow_higher_order_entrypoints]),
    Type = aere_sophia:type_of_user_input(TEnv),
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    TypeStrClean = re:replace(TypeStr, ?TYPE_CONTAINER ++ "[0-9]*\\.", "", [global, {return, list}]),
    {aere_msg:output(TypeStrClean), State};
apply_command(state, I, State) ->
    get_ready_chain(State),
    set_state(aere_sophia:parse_body(I), State);
apply_command(eval, I, State) ->
    get_ready_chain(State),
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
    end;
apply_command(load, Modules, State) ->
    get_ready_chain(State),
    load_modules(Modules, State);
apply_command(reload, [], State) ->
    get_ready_chain(State),
    reload_modules(State);
apply_command(set, [Option|Args], State) ->
    get_ready_chain(State),
    set_option(list_to_atom(Option), Args, State);
apply_command(help, Arg, State) ->
    case Arg of
        [On] -> {aere_msg:help(On), State};
        _ -> {aere_msg:help(), State}
    end;
apply_command(print, [What], State) ->
    {print_state(State, What), State};
apply_command(disas, Args, State) ->
    case aere_repl_state:blockchain_state(State) of
        {breakpoint, ES} ->
            Chain = aefa_engine_state:chain_api(ES),
            NewState = aere_repl_state:set_blockchain_state({ready, Chain}, State),
            apply_command(disas, Args, NewState);
        {running, _, _, _} ->
            apply_command(disas, Args, make_state_ready(State));
        _ ->
            [What] = Args,
            Fate = disassemble(What, State),
            {aere_msg:output(lists:flatten(aeb_fate_asm:pp(Fate))), State}
    end;
apply_command(break, [File, Line], State) ->
    OldBreakpoints = aere_repl_state:breakpoints(State),
    Breakpoint     = {File, list_to_integer(Line)},
    NewBreakpoints = OldBreakpoints ++ [Breakpoint],
    aere_repl_state:set_breakpoints(NewBreakpoints, State);
apply_command(delete_break, [IndexStr], State) ->
    Index = list_to_integer(IndexStr),
    Breakpoints = aere_repl_state:breakpoints(State),
    [ throw({repl_error, aere_msg:error("Breakpoint does not exist")})
        || Index < 1 orelse Index > length(Breakpoints) ],
    {Left, [_ | Right]} = lists:split(Index - 1, Breakpoints),
    aere_repl_state:set_breakpoints(Left ++ Right, State);
apply_command(info_break, _, State) ->
    Breakpoints = aere_repl_state:breakpoints(State),
    Msg = [ io_lib:format("~p    Breakpoint in the file '~s' at line ~p\n", [Index, File, Line]) 
            || {Index, {File, Line}} <- lists:zip(lists:seq(1, length(Breakpoints)), Breakpoints) ],
    {aere_msg:output(lists:flatten(Msg)), State};
apply_command(DebugRun, [], State)
  when DebugRun == continue;
       DebugRun == next;
       DebugRun == step;
       DebugRun == finish ->
    case aere_repl_state:blockchain_state(State) of
        {breakpoint, ES} ->
            CurFun = aefa_engine_state:current_function(ES),
            Status =
                case DebugRun of
                    S when S == next; S == finish -> {S, CurFun};
                    _                             -> DebugRun
                end,
            ES1 = aefa_engine_state:set_breakpoint_stop(false, ES),
            ES2 = aefa_engine_state:set_debugger_status(Status, ES1),
            eval_state(ES2, State);
        _ ->
            {aere_msg:error("Not at breakpoint!"), State}
    end;
apply_command(location, [], State) ->
    case aere_repl_state:blockchain_state(State) of
        {breakpoint, ES} ->
            {FileName, Line} = aefa_engine_state:debugger_location(ES),
            {aere_msg:output(exec_location(FileName, Line)), State};
        _ ->
            {aere_msg:error("Not at breakpoint!"), State}
    end;
apply_command(print_var, [VarName], State) ->
    case aere_repl_state:blockchain_state(State) of
        {breakpoint, ES} ->
            Reg = aefa_engine_state:get_variable_register(VarName, ES),
            case Reg of
                undefined -> {aere_msg:error("Undefined var"), State};
                _         ->
                    {Val, _} = aefa_fate:lookup_var(Reg, ES),
                    {aere_msg:output(integer_to_list(Val)), State}
            end;
        _ ->
            {aere_msg:error("Not at breakpoint!"), State}
    end.

%%%------------------

%% Return the source code with a mark on the currently executing line
-spec exec_location(string(), integer()) -> string().
exec_location(FileName, CurrentLine) ->
    {ok, File} = read_file(FileName),
    Lines      = string:split(File, "\n", all),
    LineSign   =
        fun(Id) when Id == CurrentLine -> ">";
           (_)                         -> "|"
        end,
    MaxDigits     = length(integer_to_list(length(Lines))),
    FormatLineNum = fun(Num) -> string:right(integer_to_list(Num), MaxDigits) end,
    FormatLine    = fun(N, Ln) -> [LineSign(N), " ", FormatLineNum(N), " ", Ln] end,
    Enumerate     = fun(List) -> lists:zip(lists:seq(1, length(List)), List) end,
    NewLines      = [ FormatLine(Idx, Line) || {Idx, Line} <- Enumerate(Lines) ],
    lists:join("\n", NewLines).

%%%------------------

-spec set_state([aeso_syntax:stmt()], repl_state()) -> aere_repl_state:command_res().
set_state(Body, RS) ->
    Contract     = aere_mock:eval_contract(Body, RS),
    {TEnv, TAst} = aere_sophia:typecheck(Contract),
    Type         = aere_sophia:type_of_user_input(TEnv),
    ByteCode     = aere_sophia:compile_contract(TAst),

    Callback = fun(State) -> set_state_callback(Type, State) end,
    NewRS    = aere_repl_state:set_callback(Callback, RS),
    run_contract(ByteCode, NewRS).

set_state_callback(Type, RS0) ->
    ChainState       = aere_repl_state:blockchain_state(RS0),
    {_, StateVal, _} = get_running_chain(ChainState),

    RS1 = aere_repl_state:set_contract_state({Type, StateVal}, RS0),
    RS2 = aere_repl_state:set_vars([], RS1),
    RS3 = aere_repl_state:set_funs(#{}, RS2),
    make_state_ready(RS3).

%%%------------------

-spec eval_expr([aeso_syntax:stmt()], repl_state()) -> aere_repl_state:command_res().
eval_expr(Body, RS) ->
    Ast          = aere_mock:eval_contract(Body, RS),
    {TEnv, TAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode     = aere_sophia:compile_contract(TAst),

    Callback = fun(State) -> eval_expr_callback(State, TEnv) end,
    NewRS    = aere_repl_state:set_callback(Callback, RS),
    run_contract(ByteCode, NewRS).

eval_expr_callback(RS, TEnv) ->
    ChainState        = aere_repl_state:blockchain_state(RS),
    {_, Res, UsedGas} = get_running_chain(ChainState),

    #{ display_gas  := DisplayGas,
       print_unit   := PrintUnit,
       print_format := PrintFormat } = aere_repl_state:options(RS),

    case {PrintUnit, Res, DisplayGas} of
        {false, {tuple, {}}, false} -> RS;
        {false, {tuple, {}}, true}  -> {aere_msg:used_gas(UsedGas), RS};
        _ ->
            Type = aere_sophia:type_of_user_input(TEnv),

            ResStr = aere_msg:output(format_value(PrintFormat, TEnv, Type, Res)),
            GasStr = [aere_msg:used_gas(UsedGas) || DisplayGas],
            {[ResStr|GasStr], make_state_ready(RS)}
    end.

%%%------------------

make_state_ready(State) ->
    case aere_repl_state:blockchain_state(State) of
        {running, Chain, _, _} ->
            aere_repl_state:set_blockchain_state({ready, Chain}, State);
        _ ->
            State
    end.

format_value(fate, _, _, Val) ->
    io_lib:format("~p", [Val]);
format_value(sophia, TEnv, Type, Val) ->
    aere_sophia:format_value(sophia, TEnv, Type, Val);
format_value(json, TEnv, Type, Val) ->
    aere_sophia:format_value(json, TEnv, Type, Val).

load_modules([], S0) ->
    S0;
load_modules(Filenames, S0) ->
    Modules = read_files(Filenames),
    S1 = register_modules(Modules, S0),
    S2 = register_include(element(1, lists:last(Modules)), S1),
    S2.

reload_modules(RS) ->
    LdFiles  = aere_repl_state:loaded_files(RS), 
    IncFiles = aere_repl_state:included_files(RS),
    Modules  = read_files(maps:keys(LdFiles)),

    RS1 = register_modules(Modules, RS),
    RS2 = lists:foldl(fun register_include/2, RS1, IncFiles),
    RS2.

% Reads files either from working dir or stdlib
read_file(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            file:read_file(filename:join(aeso_stdlib:stdlib_include_path(), Filename));
        Res -> Res
    end.

read_files(Filenames) ->
    Files = [read_file(F) || F <- Filenames],

    case [{File, file:format_error(Err)} || {File, {error, Err}} <- lists:zip(Filenames, Files)] of
        []     -> ok;
        Failed -> throw({repl_error, aere_msg:files_load_error(Failed)})
    end,

    lists:zip(Filenames, [File || {ok, File} <- Files]).

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
    Chain1 = lists:foldl(fun aefa_chain_api:remove_contract/2, Chain0, Contracts),
    put(contract_code_cache, undefined),
    S1 = aere_repl_state:set_vars([], S0),
    S2 = aere_repl_state:set_funs(#{}, S1),
    S3 = aere_repl_state:set_typedefs([], S2),
    S4 = aere_repl_state:set_type_scope([], S3),
    S5 = aere_repl_state:set_included_files([], S4),
    S6 = aere_repl_state:set_included_code([], S5),
    S6 = aere_repl_state:set_blockchain_state({ready, Chain1}, S6),
    S6.

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
                         SY = aere_repl_state:set_included_files(IncCode ++ Ast0, SX),
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

    run_contract(ByteCode, aere_repl_state:set_callback(fun(State) -> register_letval_callback(NewVars, ByteCode, TEnv, State) end, S0)).

register_letval_callback(NewVars, ByteCode, TEnv, S) -> 
    Funs = aere_repl_state:funs(S),
    {running, _, Res, _} = aere_repl_state:blockchain_state(S),
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
    Funs1 = generated_functions(ByteCode, NameMap),

    S2 = register_vars(Vars1, S1),
    S3 = aere_repl_state:set_funs(maps:merge(Funs, Funs1), S2),

    make_state_ready(S3).

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
    case parse_fun_ref(What) of
        {deployed, Expr, Name} ->
            Contract = aere_mock:eval_contract(Expr, S0),
            {_, TAst} = aere_sophia:typecheck(Contract),
            MockByteCode = aere_sophia:compile_contract(TAst),
            run_contract(MockByteCode, aere_repl_state:set_callback(fun(State) -> disassemble_callback(MockByteCode, Name, State) end, S0));
        {definition, Id} ->
            Contract = aere_mock:eval_contract(Id, S0),
            {_, TAst} = aere_sophia:typecheck(Contract, [allow_higher_order_entrypoints]),
            MockByteCode = aere_sophia:compile_contract(TAst),
            run_contract(MockByteCode, aere_repl_state:set_callback(fun(State) -> disassemble_callback(MockByteCode, none, State) end, S0));
        {local, _Name} -> throw(not_supported)
    end.

disassemble_callback(MockByteCode, Name, RS) ->
    {running, Chain, Res, _} = aere_repl_state:blockchain_state(RS),
    case Res of
        {contract, Pubkey} ->
            case aefa_chain_api:contract_fate_bytecode(Pubkey, Chain) of
                error -> throw({repl_error, aere_msg:contract_not_found()});
                {ok, Code, _, _} ->
                    SerName = aeb_fate_code:symbol_identifier(binary:list_to_bin(Name)),
                    extract_fun(Code, SerName)
            end;
        {tuple, {FName, _}} ->
            extract_fun(MockByteCode, FName)
    end.

-spec extract_fun(aeb_fate_code:fcode(), binary()) -> aeb_fate_code:fcode().
extract_fun(Code, Name) ->
    Functions = aeb_fate_code:functions(Code),
    case maps:get(Name, Functions, not_found) of
        not_found -> throw({repl_error, aere_msg:function_not_found_in(Name)});
        _ ->
            Functions1 = maps:filter(fun(F, _) -> F == Name end, Functions),
            Code0 = aeb_fate_code:new(),
            Code1 = aeb_fate_code:update_symbols(Code0, aeb_fate_code:symbols(Code)),
            Code2 = aeb_fate_code:update_functions(Code1, Functions1),
            Code2
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

run_contract(ByteCode, S) ->
    ES = setup_fate_state(ByteCode, S),
    eval_state(ES, S).

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
    catch {aefa_fate, revert, ErrMsg, _} ->
        throw({revert, ErrMsg})
    end.

setup_fate_state(ByteCode, RS) ->
    Owner = aere_repl_state:repl_account(RS),
    {ready, ChainApi0} = aere_repl_state:blockchain_state(RS),
    Vars = aere_repl_state:vars(RS),
    Funs = aere_repl_state:funs(RS),
    #{call_gas := Gas, call_value := Value} = aere_repl_state:options(RS),
    {_, StateVal} = aere_repl_state:contract_state(RS),
    Breakpoints = aere_repl_state:breakpoints(RS),

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

    setup_fate_state(aect_contracts:pubkey(Contract), ByteCode, Owner, Caller, Function, Vars, Gas, Value, StateVal, Funs, ChainApi, Breakpoints).

setup_fate_state(Contract, ByteCode, Owner, Caller, Function, Vars, Gas, Value, StateVal, Functions0, ChainApi, Breakpoints) ->
    Store = aefa_stores:initial_contract_store(),
    Functions = maps:merge(Functions0, aeb_fate_code:functions(ByteCode)),
    ES0 =
        aefa_engine_state:new_dbg(
          Gas,
          Value,
          #{caller => Owner}, % Spec
          aefa_stores:put_contract_store(Contract, Store, aefa_stores:new()),
          ChainApi,
          #{Contract => {ByteCode, aere_version:vm_version()}}, % Code cache
          aere_version:vm_version(),
          Breakpoints
         ),
    ES1 = aefa_engine_state:update_for_remote_call(Contract, ByteCode, aere_version:vm_version(), Caller, ES0),
    ES2 = aefa_fate:set_local_function(Function, false, ES1),
    ES3 = aefa_fate:bind_args([Arg || {_, _, Arg} <- Vars], ES2),
    ES4 = aefa_engine_state:set_functions(Functions, ES3),
    ES5 = aefa_fate:store_var({var, -1}, StateVal, ES4),

    ES5.

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
            FileMap = maps:from_list(read_files(Files)),
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

get_running_chain({running, Chain, Res, Gas}) ->
    {Chain, Res, Gas};
get_running_chain(_) ->
    throw({repl_error, aere_msg:chain_not_running()}).

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
