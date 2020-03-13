-module(aere_tests).

-compile([export_all, nowarn_export_all]).

-include("../src/aere_repl.hrl").
-include_lib("eunit/include/eunit.hrl").

run_test(Test) ->
    TestFun = list_to_atom(lists:concat([Test, "_test_"])),
    [ begin
          io:format("~s\n", [Label]),
          Fun()
      end || {Label, Fun} <- ?MODULE:TestFun() ],
    ok.

eval_test_() ->
    [{"Testing the " ++ TestScenario ++ " sample input",
      fun() ->
              {ok, Input} = file:read_file("test/inputs/" ++ TestScenario ++ ".aesi"),
              Results = eval(binary_to_list(Input)),
              ?assertEqual(Answers, Results)
      end} || {TestScenario, Answers} <- evaluable_inputs()].

evaluable_inputs() ->
    [ { "twoplustwo"
      , ["4"]
      }
    , { "twocommands"
      , ["4", "6"]
      }
    , { "multiline"
      , [ "4"
        , "8"
        , "\"B\""
        ]
      }
    , { "letdef"
      , [ "2"
        , "2"
        , "2"
        , "\"XDDD\""
        , "2"
        , "7"
        ]
      }
    , { "fundef"
      , [ "3"
        , "5"
        , "100"
        , "5"
        , "true"
        , "[2, 3, 4, 5, 6, 7]"
        , "[2, 3, 4, 5, 6, 7]"
        , "100000"
        ]
    }
    ].

uncolor(T) ->
    aere_color:render_colored(none, T).

eval(I) ->
    InitSt =
        begin
            Default = aerepl:init_state(),
            Default#repl_state{options = Default#repl_state.options#options{colors = none}}
        end,
    Splitted = aere_parse:split_input(I),
    Build =
        lists:foldr(
          fun(Cmd, Cont) ->
                  fun({S, AccL}) ->
                          Resp = aerepl:process_string(S, Cmd),
                          case Resp of
                              #repl_response{status = finito} ->
                                  lists:reverse(AccL);
                              #repl_response{status = {success, S1},
                                             output = ""
                                            } ->
                                  Cont({S1, AccL});
                              #repl_response{status = {success, S1},
                                             output = Out
                                            } ->
                                  Cont({S1, [uncolor(Out)|AccL]})
                          end
                  end
          end, fun({_, L}) -> lists:reverse(L) end, Splitted),
    Outputs = Build({InitSt, []}),
    Outputs.
