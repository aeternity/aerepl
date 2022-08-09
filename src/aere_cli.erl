-module(aere_cli).

-export([main/1, start/0]).

main(_Args) ->
    start().

start() ->
    erlang:system_flag(backtrace_depth, 100),
    {ok, _} = aere_gen_server:start([]),
    Banner = gen_server:call(aere_gen_server, banner),
    io:format(Banner ++ "\n\n"),
    loop().

loop() ->
    Inp = get_input(),
    {Status, Out} = aere_gen_server:input(Inp),
    print_msg(Out),
    case Status of
        finish -> ok;
        skip -> loop();
        ok -> loop();
        error -> loop();
        internal_error -> loop()
    end.

print_msg("") -> ok;
print_msg(Msg) ->
    io:format("~s\n", [Msg]).


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
