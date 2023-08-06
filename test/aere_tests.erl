-module(aere_tests).

-compile([export_all, nowarn_export_all]).

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
              Input = case file:read_file(File) of
                          {ok, I} -> I;
                          {error, enoent} -> error(File ++ " not found")
                      end,

              {Answers, Results} = eval(binary_to_list(Input)),
              aere_gen_server:reset(),

              ?IF(length(Answers) /= length(Results), ?assertEqual(Answers, Results), ok),
              Results1 =
                  [ case A of
                        any -> any;
                        _ -> R
                    end
                    || {A, R} <- lists:zip(Answers, Results)
                  ],
              ?assertEqual(Answers, Results1)
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

eval_inputs([], Outputs) ->
    lists:reverse(Outputs);
eval_inputs([Input | Rest], Outputs) ->
    case aere_gen_server:input(Input) of
        {ok, Msg} ->
            eval_inputs(Rest, [aere_theme:render(Msg) | Outputs]);
        {error, ErrMsg} ->
            eval_inputs(Rest, [aere_theme:render(ErrMsg) | Outputs]);
        no_output ->
            eval_inputs(Rest, ["" | Outputs]);
        finish ->
            eval_inputs(Rest, Outputs)
    end.

eval_inputs(Inputs) ->
    eval_inputs(Inputs, []).

eval(I) ->
    Entries = split_file(I),
    Expected = [ Output || #entry{output = Output} <- Entries],
    Inputs = [ Input || #entry{input = Input} <- Entries ],
    Received = eval_inputs(Inputs),
    {Expected, Received}.
