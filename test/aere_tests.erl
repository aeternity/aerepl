-module(aere_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

run_test(Test) ->
    TestFun = list_to_atom(lists:concat([Test, "_test_"])),
    [ begin
          io:format("~s\n", [Label]),
          Fun()
      end || {Label, Fun} <- ?MODULE:TestFun() ],
    ok.


load_paths() ->
    ScriptDir = filename:dirname(escript:script_name()),
    Paths = filelib:wildcard("_build/prod/lib/*/ebin/", ScriptDir),
    [code:add_pathz(filename:append(ScriptDir, Path))
     orelse error({not_found, filename:append(ScriptDir, Path)})
     || Path <- Paths],
    ok.

eval_test_() ->
    {setup, fun load_paths/0, [{generator, fun tests/0}]}.

tests() ->
    [{"Testing the " ++ atom_to_list(TestScenario) ++ " scenario",
      fun() ->
              File = "test/scenarios/" ++ atom_to_list(TestScenario) ++ ".aesi",
              Commands =
                  case file:consult(File) of
                      {ok, I} -> I;
                      {error, enoent} -> error(File ++ " not found")
                  end,

              test_scenario(TestScenario, Commands)
      end} || TestScenario <- scenarios()] ++
    [ { "Testing the uniqueness of long repl commands",
        fun() ->
            Commands = [ Long || {Long, _} <- aere_parse:commands() ],
            io:format("Duplicate long commands found"),
            ?assertEqual([], duplicated(Commands))
        end } ] ++
    [ { "Testing the uniqueness of short repl commands",
        fun() ->
            io:format("Duplicate short commands found"),
            Commands = [ Short || {_, {[Short], _, _, _}} <- aere_parse:commands() ],
            ?assertEqual([], duplicated(Commands))
        end } ].


test_scenario(Scenario, Commands) ->
    {ok, REPL} = aere_gen_server:start_link({local, Scenario}, []),
    try test_commands(REPL, Commands)
    after aere_gen_server:stop(REPL)
    end.


validate_output(succeed, Output) ->
    io:format("Expecting success:\n~p\n", [Output]),
    ?assertNotMatch({error, _}, Output);

validate_output({error, Err}, Output = {error, GotErr}) ->
    io:format("Expecting error:\n~p\n", [Err]),
    io:format("Got:\n~p\n", [GotErr]),
    ?assertEqual({error, Err}, Output);

validate_output({error_render, ErrMsg}, Output) when is_list(ErrMsg )->
    validate_output({error_render, list_to_binary(ErrMsg)}, Output);

validate_output({error_render, ErrMsg}, Output) ->
    io:format("Expecting error message:\n~s\n", [ErrMsg]),
    Fmt = aere_msg:format_err(Output),
    Str = aere_theme:render(Fmt),
    io:format("Got:\n~s\n", [Str]),
    ?assertEqual(ErrMsg, Str);

validate_output({expect, Expect}, Output) ->
    io:format("Expecting output:\n~p\n", [Expect]),
    io:format("Got:\n~p\n", [Output]),
    ?assertEqual(Expect, Output);

validate_output({match, Match}, Output) ->
    validate_output({match, Match, []}, Output);

validate_output({match, Match, Guards}, Output) ->
    io:format("Expecting match:\n~p\n", [Match]),
    io:format("With guards:\n~p\n", [Guards]),
    io:format("Got:\n~p\n", [Output]),
    %% TODO: do we really need ETS for that?
    T = ets:new(match_test, []),
    ets:insert(T, {match_success, Output}),
    Result = ets:select(T, [{{'$0', Match}, Guards, ['$0']}]),
    try ?assertEqual([match_success], Result)
    after ets:delete(T),
          ok
    end;

validate_output({render, Render}, Output) when is_list(Render) ->
    validate_output({render, list_to_binary(Render)}, Output);

validate_output({render, Render}, Output) ->
    io:format("Expecting output message:\n~p\n", [Render]),
    Fmt = aere_msg:format(Output),
    Str = aere_theme:render(Fmt),
    io:format("Got:\n~s\n", [Str]),
    ?assertEqual(Render, Str).


test_commands(REPL, [Command | Rest]) when is_list(Command) ->
    test_commands(REPL, [{Command, succeed} | Rest]);
test_commands(REPL, [{Command, Test}|Rest]) ->
    io:format("\n*** Command: ~s\n", [Command]),
    Output = eval(REPL, Command),
    validate_output(Test, Output),
    test_commands(REPL, Rest);
test_commands(_REPL, []) ->
    ok.


-spec duplicated(list()) -> list().
duplicated(List) ->
    Keys  = lists:usort(List),
    Count = fun(V,L) -> length(lists:filter(fun(E) -> E == V end, L)) end,
    Freq  = [ { K, Count(K, List) } || K <- Keys ],
    IsDup = fun({K, F}) when F > 1 -> {true, K};
               (_)                 -> false
            end,
    lists:filtermap(IsDup, Freq).

scenarios() ->
    [ basic_usage
    , type_command
    , state_command
    , print_command
    , load_command
    , reload_command
    , all_loading_commands
    , reset_command
    , options_setting
    , stack_trace_abort
    , debugger_basic_usage
    , accounts
    ].

eval(REPL, I) ->
    Output = aere_gen_server:input(REPL, I),
    Output.
