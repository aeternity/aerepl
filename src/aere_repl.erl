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
       blockchain_state = ChainState,
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
                  {error, Msg} ->
                    #repl_response
                        { output = render_msg(State, Msg)
                        , warnings = []
                        , status = error
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
    Contract = aere_mock:mock_contract(Stmts),
    TAst = aere_sophia:typecheck([Contract], [dont_unfold]),
    {_, Type} = aere_sophia:type_of(TAst, ?USER_INPUT),
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    {aere_color:output(TypeStr), State};
process_input(State, eval, I) ->
    Parse = aere_sophia:parse_top(I),
    case Parse of
        {body, Body} ->
            eval_contract(Body, State)
    end;
process_input(_, _, _) ->
    throw(aere_error:undefined_command()).

-spec eval_contract([aeso_syntax:stmt()], repl_state()) -> {colored(), repl_state()}.
eval_contract(Body, S) ->
    Ast = [aere_mock:mock_contract(Body)],
    TypedAst = aere_sophia:typecheck(Ast),
    ByteCode = aere_sophia:compile_contract(TypedAst),

    ES0 = setup_fate_state(ByteCode, S),
    ES1 = aefa_fate:execute(ES0),
    Res = aefa_engine_state:accumulator(ES1),
    ChainApi1 = aefa_engine_state:chain_api(ES1),

    ResStr = io_lib:format("~p", [Res]),

    {aere_color:output(ResStr), S#repl_state{blockchain_state = ChainApi1}}.

setup_fate_state(ByteCode, #repl_state{repl_account = Owner, blockchain_state = ChainApi}) ->

    Store = aect_contracts_store:new(),
    Function = aeb_fate_code:symbol_identifier(<<?USER_INPUT>>),

    Caller = aeb_fate_data:make_address(Owner),

    setup_fate_state(Owner, ByteCode, Owner, Caller, Function, 100000000, 0, Store, ChainApi).

setup_fate_state(Contract, ByteCode, Owner, Caller, Function, Gas, Value, Store, ChainApi) ->
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
    ES1 = aefa_engine_state:update_for_remote_call(Owner, ByteCode, aere_version:vm_version(), Caller, ES0),
    ES2 = aefa_fate:set_local_function(Function, false, ES1),

    ES2.
