%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ init_state/0, process_string/2
        , print_msg/2, render_msg/1, banner/0
        ]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

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
    #repl_state{ blockchain_state = ChainState
               , user_account = PK
               }.


-spec banner() -> string().
banner() ->
"
    ____
   / __ | ,             _     _
  / / |_|  )           | |   (_)
 ( (_____,-` ___  _ __ | |__  _  __ _
  \\______ \\ / _ \\| '_ \\| '_ \\| |/ _` |
  ,-`    ) ) (_) ) |_) ) | | | | (_| |
 (  ____/ / \\___/| .__/|_| |_|_|\\__,_|
  `(_____/       | |
                 |_|  interactive

".

%% Renders colored and untrimmed text into a string
-spec render_msg(colored()) -> string().
render_msg(Msg) ->
    lists:flatten(aere_color:render_colored(Msg)).


%% Renders and prints message
-spec print_msg(repl_state(), colored()) -> ok.
print_msg(_, "") -> ok;
print_msg(_, "&#wololo#&") ->
    put(wololo, wololo), ok;
print_msg(S, M) ->
    Render = render_msg(M),
    io:format("~s\n", [string:trim(Render, both, aere_parse:whitespaces())]).

%% state + input = response
-spec process_string(repl_state(), binary() | string())
                    -> repl_response() | finito.
process_string(State, String) when is_binary(String) ->
    process_string(State, binary_to_list(String));
process_string(State, String) ->
    Proc = aere_parse:dispatch(String),
    handle_dispatch(State, Proc).


%% Handle result of REPL command dispatcher
-spec handle_dispatch(repl_state(), skip | {ok, {command, string()}})
                     -> repl_response().
handle_dispatch(State, skip) ->
    #repl_response
        { output = ""
        , warnings = []
        , status = {success, State}
        };
handle_dispatch(State = #repl_state{}, {ok, {Command, Args}}) ->
    to_response(State,
        fun() ->
            case process_input(State, Command, Args) of
                NewState = #repl_state{}        -> NewState;
                {Msg, NewState = #repl_state{}} -> {Msg, NewState};
                Res                             -> Res
            end
        end);
handle_dispatch(State, {error, {no_such_command, "wololo"}}) ->
    put(wololo, wololo),
    #repl_response{output = "&#wololo#&", warnings = [], status = {success, State}};
handle_dispatch(#repl_state{}, {error, {no_such_command, Command}}) ->
    Msg = aere_error:no_such_command(Command),
    #repl_response
        { output = Msg
        , warnings = []
        , status = error
        };
handle_dispatch(#repl_state{}, {error, {ambiguous_prefix, Propositions}}) ->
    Msg = aere_error:ambiguous_prefix(Propositions),
    #repl_response
        { output = Msg
        , warnings = []
        , status = error
        }.

-spec to_response(repl_state(), fun(() -> finito | {string(), repl_state()} | repl_state() | none()))
                 -> repl_response().
to_response(FallbackState, Action) ->
    try Action() of
        {Out, State1 = #repl_state{}} ->
            #repl_response
                { output = Out
                , warnings = []
                , status = {success, State1}
                };
        State1 = #repl_state{} ->
            #repl_response
                { output = ""
                , warnings = []
                , status = {success, State1}
                };
        skip ->
            #repl_response
                { output = ""
                , warnings = []
                , status = {success, FallbackState}
                };
        finito ->
            #repl_response
                { output = ""
                , warnings = []
                , status = finito
                };
        WTF ->
            Msg = aere_color:emph(
                    aere_color:red(
                      io_lib:format("Unexpected dispatch: ~p", [WTF]))),
            #repl_response
                { output = Msg
                , warnings = []
                , status = internal_error
                }
    catch error:E:Stacktrace ->
            Msg = aere_error:internal(E, Stacktrace),
            #repl_response
                { output = Msg
                , warnings = []
                , status = internal_error
                };
          {error, Msg} ->
            #repl_response
                { output = Msg
                , warnings = []
                , status = error
                }
    end.

%% Specific reactions to commands and inputs
-spec process_input(repl_state(), aere_parse:command(), string()) ->
          finito | {string(), repl_state()} | repl_state() | none().
process_input(_, quit, _) ->
    finito;
process_input(_, reset, _) ->
    init_state();
process_input(State, type, I) ->
    Stmts = aere_sophia:parse_body(I),
    Contract = aere_mock:mock_contract(Stmts),
    TAst = aere_sophia:typecheck([Contract], [dont_unfold]),
    {_, Type} = aere_sophia:type_of(TAst, ?USER_INPUT),
    {aeso_ast_infer_types:pp_type("", Type), State};
process_input(State, eval, I) ->
    Parse = aere_sophia:parse_top(I),
    case Parse of
        {body, Body} ->
            eval_contract(Body, State)
    end;
process_input(_, _, _) ->
    throw(aere_error:undefined_command()).

eval_contract(Body, S = #repl_state{user_account = Owner, blockchain_state = ChainApi}) ->
    ContractPubkey = Owner,
    Store = aect_contracts_store:new(),
    Function = aeb_fate_code:symbol_identifier(<<?USER_INPUT>>),

    Ast = [aere_mock:mock_contract(Body)],
    TypedAst = aere_sophia:typecheck(Ast),
    ByteCode = aere_sophia:compile_contract(TypedAst),
    ES0 =
        aefa_engine_state:new(
          10000000000000000000, % Gas
          0, % Value
          #{caller => Owner}, % Spec
          aefa_stores:put_contract_store(ContractPubkey, Store, aefa_stores:new()),
          ChainApi,
          #{}, % Code cache
          aere_version:vm_version()
         ),
    Arguments = [],
    Contract = aeb_fate_data:make_contract(ContractPubkey),
    Stores =
    ES1 = ES0,
    Caller = aeb_fate_data:make_address(Owner),
    ES2 = aefa_engine_state:update_for_remote_call(Owner, ByteCode, aere_version:vm_version(), Caller, ES1),
    ES3 = aefa_fate:set_local_function(Function, false, ES2),
    io:format("~p", [ByteCode]),
    EngineStateAfter = aefa_fate:execute(ES3),
    Res = aefa_engine_state:accumulator(EngineStateAfter),
    {io_lib:format("~p", [Res]), S#repl_state{blockchain_state = aefa_engine_state:chain_api(EngineStateAfter)}}.
