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
    aere_gen_server:start_link([]),
    [{"Testing the " ++ atom_to_list(TestScenario) ++ " scenario",
      fun() ->
              File = "test/scenarios/" ++ atom_to_list(TestScenario) ++ ".aesi",
              Commands =
                  case file:consult(File) of
                      {ok, I} -> I;
                      {error, enoent} -> error(File ++ " not found")
                  end,

              test_commands(Commands)
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

validate_output(succeed, Output) ->
    io:format("Expecting success:\n~p\n", [Output]),
    ?assertNotMatch({error, _}, Output);
validate_output({error, Err}, Output) ->
    io:format("Expecting error:\n~p\n", [Err]),
    io:format("Got:\n~p\n", [Output]),
    ?assertEqual({error, Err}, Output);
validate_output({expect, Expect}, {ok, Output}) ->
    io:format("Expecting output:\n~p\n", [Expect]),
    io:format("Got:\n~p\n", [Output]),
    ?assertEqual(Expect, Output);
validate_output({match, Match}, Output) ->
    validate_output({match, Match, []}, Output);
validate_output({match, Match, Guards}, {ok, Output}) ->
    io:format("Expecting match:\n~p\n", [Match]),
    io:format("With guards:\n~p\n", [Guards]),
    io:format("Got:\n~p\n", [Output]),
    %% TODO: do we really need ETS for that?
    T = ets:new(match_test, []),
    ets:insert(T, {output, Output}),
    Result = ets:select(T, [{{'$0', Match}, Guards, ['$0']}]),
    try ?assertEqual([output], Result)
    after ets:delete(T)
    end.


test_commands([Command | Rest]) when is_list(Command) ->
    test_commands([{Command, succeed} | Rest]);
test_commands([{Command, Test}|Rest]) ->
    io:format("\n*** Command: ~s\n", [Command]),
    validate_output(Test, eval(Command)),
    test_commands(Rest);
test_commands([]) ->
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
    ].

eval(I) ->
    aere_gen_server:input(I).
