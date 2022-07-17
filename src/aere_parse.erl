-module(aere_parse).

-export([ parse/1, get_input/0, words/1 ]).

-type parse_result() :: {ok, {aere_repl:command(), string()}}
                      | {error, {no_such_command, string()}}
                      | skip.

-spec commands() -> list(aere_repl:command()).
commands() ->
    [ quit, type, eval, include, load, reload, add, set ].

%% Parse an input string. This function is called on strings entered by the user in the repl
-spec parse(string()) -> parse_result().
parse(Input) ->
    case Input of
        []  -> skip;
        ":" -> skip;
        [$:|CommandAndArg] ->
            [Command | _] = string:tokens(CommandAndArg, unicode_util:whitespace()),
            Arg = string:trim(CommandAndArg -- Command, leading, unicode_util:whitespace()),
            KnownCommands = [atom_to_list(C) || C <- commands()],
            case lists:member(Command, KnownCommands) of
                true  -> {ok, {list_to_existing_atom(Command), Arg}};
                false -> {error, {no_such_command, Command}}
            end;
        _ ->
            %% Eval is the default command (i.e. 1 + 1 is just :eval 1 + 1)
            {ok, {eval, Input}}
    end.

%% Get single line or multiline input from the user and return it as a single string
-spec get_input() -> string().
get_input() ->
    Line =
        case io:get_line("AESO> ") of
            eof          -> ":quit"; % that's dirty
            {error, Err} -> exit(Err);
            Data         -> Data
        end,
    Input =
        case string:trim(Line, both, unicode_util:whitespace()) of
            ":{" -> multiline_input();
            ""   -> "";
            _    -> lists:flatten(string:replace(Line, ";", "\n", all))
        end,
    string:trim(Input, both, unicode_util:whitespace()).

-spec multiline_input() -> string().
multiline_input() -> multiline_input([]).

%% Keep reading input lines until :} is found. Return the code between :{ and :} as a single string
-spec multiline_input([string()]) -> string().
multiline_input(CodeBlock) ->
    Line = io:get_line("| "),
    case string:trim(Line, both, unicode_util:whitespace()) of
        ":}" -> lists:flatten(lists:reverse(CodeBlock));
        _    -> multiline_input([Line|CodeBlock])
    end.

words(String) ->
    string:lexemes(String, unicode_util:whitespace()).
