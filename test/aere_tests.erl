-module(aere_tests).

-compile([export_all, nowarn_export_all]).

-include("../src/aere_repl.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(entry,
        { input  :: [string()]
        , output :: [string()]
        }).
-type raw_entry() :: {raw_entry, [string()]}.
-type entry() :: #entry{}.

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
    [ basic_usage
    , type_command
    , state_command
    , print_command
    , load_command
    , reload_command
    , add_command
    , all_loading_commands
    , reset_command
    , options_setting
    ].

%% Split a file into entries
-spec split_file(string()) -> [entry()].
split_file(File) ->
    Lines = string:split(File, "\n", all),
    TrimmedLines = [ string:trim(Line, both, unicode_util:whitespace()) || Line <- Lines ],
    RawEntries = lines_to_raw_entries(TrimmedLines),
    [ split_raw_entry(RawEntry) || RawEntry <- RawEntries ].

%% Group each set of consecutive non-empty lines into a raw entry, where a raw entry
%% is a list of lines representing a single repl input and its corresponding output
-spec lines_to_raw_entries([string()]) -> [raw_entry()].
lines_to_raw_entries(Lines) -> lines_to_raw_entries(Lines, []).

-spec lines_to_raw_entries([string()], [raw_entry()]) -> [raw_entry()].
lines_to_raw_entries([], Entries) ->
    lists:reverse(Entries);
lines_to_raw_entries(Lines, Entries) ->
    IsEmpty = fun(L) -> L =:= [] end,
    IsNotEmpty = fun(L) -> L =/= [] end,

    NotEmpty = lists:dropwhile(IsEmpty, Lines),
    {Entry, Rest} = lists:splitwith(IsNotEmpty, NotEmpty),
    lines_to_raw_entries(Rest, [{raw_entry, Entry} | Entries]).

%% Split an entry into an input and a clean output (without the // prefix)
-spec split_raw_entry(raw_entry()) -> entry().
split_raw_entry({raw_entry, EntryLines}) ->
    IsNotComment = fun(Line) -> not lists:prefix("//", Line) end,
    {Input, Output} = lists:splitwith(IsNotComment, EntryLines),
    UncommentOutput = [ string:trim(OutputLine, leading, "/")
                        || OutputLine <- Output ],
    CleanOutput = [ string:trim(OutputLine, leading, unicode_util:whitespace())
                    || OutputLine <- UncommentOutput ],
    #entry{ input  = lists:flatten(lists:join("\n", Input))
          , output = lists:flatten(lists:join("\n", CleanOutput)) }.

eval_inputs(_State, [], Outputs) ->
    lists:reverse(Outputs);
eval_inputs(State, [Input | Rest], Outputs) ->
    case aere_repl:process_input(State, Input) of
        #repl_response{output = Received, status = {ok, NewState}} ->
            Rendered = aere_theme:render(Received),
            eval_inputs(NewState, Rest, [Rendered  | Outputs]);
        #repl_response{output = Received, status = Err}
          when Err == error orelse Err == internal_error ->
            Rendered = aere_theme:render(Received),
            eval_inputs(State, Rest, [Rendered | Outputs]);
        _ ->
            eval_inputs(State, Rest, Outputs)
    end.

eval_inputs(State, Inputs) ->
    eval_inputs(State, Inputs, []).

eval(I) ->
    State = aere_repl:init_state(),
    Entries = split_file(I),
    Expected = [ Output || #entry{output = Output} <- Entries],
    Inputs = [ Input || #entry{input = Input} <- Entries ],
    Received = eval_inputs(State, Inputs),
    {Expected, Received}.
