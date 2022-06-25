%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ init_state/0, process_string/2, banner/1
        ]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

-spec init_options() -> repl_options().
init_options() ->
    #repl_options{
       coloring = aere_color:coloring_default()
      }.

-spec init_state() -> repl_state().
init_state() ->
    {PK, Trees} = aere_chain:new_account(100000000000000000000000000000, #{}),
    ChainState = aefa_chain_api:new(
                   #{ gas_price => 1,
                      fee => 0,
                      trees => Trees,
                      origin => PK,
                      tx_env => aere_chain:default_tx_env(1)
                   }
                  ),
    #repl_state{
       blockchain_state = {ready, ChainState},
       repl_account = PK,
       options = init_options()
      }.

banner(State) ->
    Sophia =
        "    ____\n"
        "   / __ | ,             _     _\n"
        "  / / |_|  )           | |   (_)\n"
        " ( (_____,-` ___  _ __ | |__  _  __ _\n"
        "  \\______ \\ / _ \\| '_ \\| '_ \\| |/ _` |\n"
        "  ,-`    ) ) (_) ) |_) ) | | | | (_| |\n"
        " (  ____/ / \\___/| .__/|_| |_|_|\\__,_|\n"
        "  `(_____/       | |\n"
        "                 |_|",
    Interactive = "interactive",

    SophiaC = aere_color:banner(Sophia),
    InteractiveC = aere_color:banner_sub(Interactive),

    render_msg(State, [SophiaC, "  ", InteractiveC]).

%% Renders text by applying coloring and trimming whitespaces
-spec render_msg(repl_options() | repl_state(), colored()) -> string().
render_msg(_, "") -> "";
render_msg(#repl_state{options = Opts}, Msg) ->
    render_msg(Opts, Msg);
render_msg(#repl_options{coloring = Coloring}, Msg) ->
    Render = lists:flatten(aere_color:render_colored(Coloring, Msg)),
    string:trim(Render, both, aere_parse:whitespaces()).

%% state + input = response
-spec process_string(repl_state(), binary() | string()) -> repl_response().
process_string(State, String) when is_binary(String) ->
    process_string(State, binary_to_list(String));
process_string(State, String) ->
    check_wololo(String),
    case aere_parse:dispatch(String) of
        {ok, {Command, Args}} ->
            try process_input(State, Command, Args) of
                {Out, State1 = #repl_state{}} ->
                    #repl_response
                        { output = render_msg(State1, Out)
                        , warnings = []
                        , status = {ok, State1}
                        };
                State1 = #repl_state{} ->
                    #repl_response
                        { output = ""
                        , warnings = []
                        , status = {ok, State1}
                        };
                finish ->
                    #repl_response
                        { output = "bye!"
                        , warnings = []
                        , status = finish
                        }
            catch error:E:Stacktrace ->
                    Msg = render_msg(State, aere_error:internal(E, Stacktrace)),
                    #repl_response
                        { output = Msg
                        , warnings = []
                        , status = internal_error
                        };
                  error:E ->
                    Msg = render_msg(State, aere_error:internal(E)),
                    #repl_response
                        { output = Msg
                        , warnings = []
                        , status = internal_error
                        };
                  {aefa_fate, revert, ErrMsg, _} ->
                    Msg = aere_color:error("ABORT: ") ++ aere_color:info(ErrMsg),
                    #repl_response
                        { output = render_msg(State, Msg)
                        , warnings = []
                        , status = error
                        };
                  {aefa_fate, FateErr, _} ->
                    Msg = aere_color:error(FateErr),
                    #repl_response
                        { output = render_msg(State, Msg)
                        , warnings = []
                        , status = internal_error
                        }
            end;
        {error, Error} ->
            Msg = case Error of
                      {no_such_command, Command} -> aere_error:no_such_command(Command);
                      {ambiguous_prefix, Commands} -> aere_error:ambiguous_prefix(Commands)
                  end,
            #repl_response
                { output = render_msg(State, Msg)
                , warnings = []
                , status = error
                }
    end.

check_wololo(String) ->
    case string:find(String, "wololo") of
        nomatch ->
            ok;
        _ ->
            put(wololo, wololo),
            ok
    end.

%% Specific reactions to commands and inputs
-spec process_input(repl_state(), aere_parse:command(), string()) -> command_res().
process_input(_, quit, _) ->
    finish;
process_input(_, reset, _) ->
    init_state();
process_input(State, type, I) ->
    Stmts = aere_sophia:parse_body(I),
    Contract = aere_mock:eval_contract(Stmts, State),
    TAst = aere_sophia:typecheck(Contract, [dont_unfold]),
    {_, Type} = aere_sophia:type_of(TAst, ?USER_INPUT),
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    {aere_color:output(TypeStr), State};
process_input(State, eval, I) ->
    Parse = aere_sophia:parse_top(I),
    case Parse of
        {body, Body} ->
            eval_expr(Body, State);
        [{include, _, {string, _, Inc}}] ->
            throw(unimplemented);
        [{letval, _, Pat, Expr}] ->
            register_letval(Pat, Expr, State);
        [{letfun, _, FName, Args, _, Body}] ->
            register_letfun(FName, Args, Body, State)
    end;
process_input(State = #repl_state{blockchain_state = BS}, continue, _) ->
    case BS of
        {ready, _} ->
            {aere_color:error("Not at breakpoint!"), State};
        {breakpoint, ES} ->
            Stack = aefa_engine_state:accumulator_stack(ES),
            StackS = io_lib:format("~p", [Stack]),
            {StackS, State}
    end;

process_input(_, _, _) ->
    throw(aere_error:undefined_command()).

-spec eval_expr([aeso_syntax:stmt()], repl_state()) -> command_res().
eval_expr(Body, S0) ->
    Ast = aere_mock:eval_contract(Body, S0),
    {Res, UsedGas, S1} = compile_and_run_contract(Ast, S0),
    ResStr = io_lib:format("~p", [Res]),
    GasStr = case (S0#repl_state.options)#repl_options.display_gas of
                 false -> "";
                 true -> io_lib:format("\nUSED GAS: ~p", [UsedGas])
             end,
    {aere_color:output(ResStr) ++ aere_color:info(GasStr), S1}.

register_letval(Pat, Expr, S0 = #repl_state{funs = Funs}) ->
    NewVars = lists:filter(
                fun(Var) -> Var /= "_" end,
                aeso_syntax_utils:used_ids({letfun, 0, 0, [], 0, Pat})), % hack: used_ids require decl and then expr
    Ast = aere_mock:letval_contract(Pat, NewVars, Expr, S0),
    TypedAst = aere_sophia:typecheck(Ast, []),
    ByteCode = aere_sophia:compile_contract(TypedAst),

    {_, {tuple_t, _, [_|Types]}} = aere_sophia:type_of(TypedAst, ?USER_INPUT),

    {{tuple,ValsPack}, _, S1} = run_contract(ByteCode, S0),
    [ <<?LETVAL_INDICATOR>>|Vals] = tuple_to_list(ValsPack),

    NameMap = build_fresh_name_map(ByteCode),
    Vals1 = replace_function_name(Vals, NameMap),

    Vars1 = lists:zip3(NewVars, Types, Vals1),
    Funs1 = generated_functions(ByteCode, NameMap),

    S2 = register_vars(Vars1, S1),

    S2#repl_state{funs = maps:merge(Funs, Funs1)}.

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

register_letfun(Name = {id, _, SName}, Args, Body, S0 = #repl_state{vars = Vars, funs = Funs}) ->
    Ast = aere_mock:letfun_contract(Name, Args, Body, S0),
    TypedAst = aere_sophia:typecheck(Ast, []),
    ByteCode = aere_sophia:compile_contract(TypedAst),

    {_, {fun_t, Ann, [], ArgsT0, RetT}} = aere_sophia:type_of(TypedAst, ?USER_INPUT),
    Type = {fun_t, Ann, [], tl(ArgsT0), RetT}, % Remove closure

    NameMap = build_fresh_name_map(ByteCode),

    Funs1 = generated_functions(ByteCode, NameMap),

    FunClosure = make_closure(Vars),
    FunNewName = maps:get(aeb_fate_code:symbol_identifier(binary:list_to_bin(SName)), NameMap),
    FunVal = {tuple, {FunNewName, FunClosure}},

    S1 = register_vars([{SName, Type, FunVal}], S0),

    S1#repl_state{funs = maps:merge(Funs, Funs1)}.

register_vars(NewVars, S = #repl_state{vars = OldVars}) ->
    Filtered = [V || V = {Name, _, _} <- OldVars, [] =:= proplists:lookup_all(Name, NewVars)],
    S#repl_state{vars = NewVars ++ Filtered}.

make_closure([{_, _, V}]) ->
    V;
make_closure(Vars) ->
    {tuple, list_to_tuple([V || {_, _, V} <- Vars])}.

-spec compile_and_run_contract([aeso_syntax:ast()], repl_state()) -> {term(), repl_state()}.
compile_and_run_contract(Ast, S) ->
    TypedAst = aere_sophia:typecheck(Ast),
    ByteCode = aere_sophia:compile_contract(TypedAst),
    run_contract(ByteCode, S).

run_contract(ByteCode, S) ->
    ES0 = setup_fate_state(ByteCode, S),
    eval_state(ES0, S).

eval_state(ES0, S) ->
    ES1 = aefa_fate:execute(ES0),

    Res       = aefa_engine_state:accumulator(ES1),
    ChainApi  = aefa_engine_state:chain_api(ES1),
    UsedGas   = (S#repl_state.options)#repl_options.call_gas - aefa_engine_state:gas(ES1),
    Break     = aefa_engine_state:at_breakpoint(ES1),

    case Break of
        true -> {"BREAK", UsedGas, S#repl_state{blockchain_state = {breakpoint, ES1}}};
        false -> {Res, UsedGas, S#repl_state{blockchain_state = {ready, ChainApi}}}
    end.

setup_fate_state(
  ByteCode,
  #repl_state{
     repl_account = Owner,
     blockchain_state = {ready, ChainApi},
     vars = Vars,
     funs = Funs,
     options = #repl_options{call_gas = Gas}
    }) ->

    Store = aect_contracts_store:new(),
    Function = aeb_fate_code:symbol_identifier(<<?USER_INPUT>>),

    Caller = aeb_fate_data:make_address(Owner),

    setup_fate_state(Owner, ByteCode, Owner, Caller, Function, Vars, Gas, _Value = 0, Store, Funs, ChainApi).

setup_fate_state(Contract, ByteCode, Owner, Caller, Function, Vars, Gas, Value, Store, Functions0, ChainApi) ->

    Functions = maps:merge(Functions0, aeb_fate_code:functions(ByteCode)),

    ES0 =
        aefa_engine_state:new(
          Gas,
          Value,
          #{caller => Owner}, % Spec
          aefa_stores:put_contract_store(Contract, Store, aefa_stores:new()),
          ChainApi,
          #{}, % Code cache
          aere_version:vm_version()
         ),
    ES1 = aefa_engine_state:update_for_remote_call(Contract, ByteCode, aere_version:vm_version(), Caller, ES0),
    ES2 = aefa_fate:set_local_function(Function, false, ES1),
    ES3 = aefa_fate:bind_args([Arg || {_, _, Arg} <- Vars], ES2),
    ES4 = aefa_engine_state:set_functions(Functions, ES3),

    ES4.
