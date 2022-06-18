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

load_deps() ->
    code:add_pathz("node/_build/dev1/lib/aechannel/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aecontract/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aecore/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aefate/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aens/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aeoracle/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aeprimop/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aetx/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aeutils/ebin/"),
    code:add_pathz("node/_build/dev1/lib/setup/ebin/"),
    application:load(aechannel),
    application:load(aecontract),
    application:load(aecore),
    application:load(aefate),
    application:load(aens),
    application:load(aeoracle),
    application:load(aeprimop),
    application:load(aetx),
    application:load(aeutils),
    application:load(setup).

eval_test_() ->
    {setup, fun load_deps/0, [{generator, fun tests/0}]}.

tests() ->
    [{"Testing the " ++ atom_to_list(TestScenario) ++ " scenario",
      fun() ->
              File = "test/scenarios/" ++ atom_to_list(TestScenario) ++ ".aesi",
              Input = case file:read_file(File) of
                          {ok, I} -> I;
                          {error, enoent} -> error(File ++ " not found")
                      end,

              {Answers, Results} = eval(binary_to_list(Input)),

              ?IF(length(Answers) /= length(Results), ?assertEqual(Answers, Results), ok),
              Results1 =
                  [ case A of
                        any -> any;
                        _ -> R
                    end
                    || {A, R} <- lists:zip(Answers, Results)
                  ],
              ?assertEqual(Answers, Results1)
      end} || TestScenario <- scenarios()].

scenarios() ->
    [ twoplustwo

    ].

format(T) ->
    string:trim(aere_color:render_colored(none, T)).

eval(I) ->
    InitSt = aere_repl:init_state(),
    Splitted = aere_parse:split_input(I),
    Build =
        lists:foldr(
          fun("", Cont) -> Cont;
             ([$/,$/|Ans], Cont) ->
                  fun({S, {Answers, Inputs}}) ->
                          case string:trim(Ans) of
                              "_" -> Cont({S, {[any|Answers], Inputs}});
                              "!error" -> Cont({S, {[error|Answers], Inputs}});
                              A -> Cont({S, {[A|Answers], Inputs}})
                          end
                  end;
             (Cmd, Cont) ->
                  fun({S, {Answers, Inputs}}) ->
                          Resp = aere_repl:process_string(S, Cmd),
                          case Resp of
                              #repl_response{status = finito} ->
                                  {lists:reverse(Answers), lists:reverse(Inputs)};
                              #repl_response{status = {success, S1},
                                             output = ""
                                            } ->
                                  Cont({S1, {Answers, Inputs}});
                              #repl_response{status = {success, S1},
                                             output = Out
                                            } ->
                                  Cont({S1, {Answers, [format(Out)|Inputs]}});
                              #repl_response{status = error} ->
                                  Cont({S, {Answers, [error|Inputs]}});
                              #repl_response{status = internal_error,
                                             output = Out
                                            } ->
                                  io:format(format(Out)),
                                  error(internal_error)
                          end
                  end
          end, fun({_, {As, Is}}) -> {lists:reverse(As), lists:reverse(Is)} end, Splitted),
    Outputs = Build({InitSt, {[], []}}),
    Outputs.
