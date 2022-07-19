-module(aere_msg).

-export([ banner/0
        , output/1
        , output_with_gas/2
        , error/1
        , abort/1
        , internal/1
        , internal/2
        , no_such_command/1
        , file_not_loaded/1
        , files_load_error/1
        , set_nothing/0
        , option_usage/2
        , bye/0
        ]).

-type msg() :: aere_theme:renderable().

-spec banner() -> msg().
banner() ->
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

    SophiaThemed = aere_theme:banner(Sophia),
    InteractiveThemed = aere_theme:banner_sub(Interactive),

    [SophiaThemed, aere_theme:output("  "), InteractiveThemed].

-spec output(string()) -> msg().
output(Msg) -> aere_theme:output(Msg).

-spec output_with_gas(string(), integer()) -> msg().
output_with_gas(Msg, UsedGas) ->
    GasMsg = io_lib:format("\nUSED GAS: ~p", [UsedGas]),
    [aere_theme:output(Msg), aere_theme:info(GasMsg)].

-spec error(string()) -> msg().
error(Msg) -> aere_theme:error(Msg).

-spec abort(string()) -> msg().
abort(Msg) -> [aere_theme:error("ABORT: "), aere_theme:info(Msg)].

-spec internal(term(), erlang:stacktrace()) -> msg().
internal(Error, Stacktrace) ->
    [ aere_theme:output("Command failed:\n")
    , aere_theme:error(lists:flatten(io_lib:format("~p", [Error])))
    , aere_theme:output("\n")
    , case Stacktrace of
          [] -> aere_theme:output("<no stacktrace>");
          _  -> aere_theme:output("Stacktrace:\n" ++ io_lib:format("~p", [Stacktrace]))
      end
    ].

-spec internal(term()) -> msg().
internal(Error) ->
    internal(Error, []).

-spec no_such_command(aere_repl:command()) -> msg().
no_such_command(Command) ->
    [ aere_theme:output("No such command ")
    , aere_theme:command(io_lib:format("~p", [Command]))
    , aere_theme:output("\n")
    ].

-spec file_not_loaded(FileName) -> msg() when
      FileName :: string().
file_not_loaded(File) ->
    [aere_theme:error("File not loaded: "), aere_theme:file(File)].

-spec files_load_error([{FileName, Reason}]) -> msg() when
      FileName :: string(),
      Reason :: string().
files_load_error(Failed) ->
    FilesAndReasons = [ [ aere_theme:file(File)
                        , aere_theme:output(": ")
                        , aere_theme:output(Err)
                        ] || {File, Err} <- Failed ],
    FlatList = lists:flatten(lists:join(aere_theme:output("\n"), FilesAndReasons)),
    [aere_theme:error("Could not load files:\n") | FlatList].

-spec set_nothing() -> msg().
set_nothing() ->
    [aere_theme:error("Please specify what to set")].

-spec option_usage(atom, [{atom, term()}]) -> msg().
option_usage(Option, Rules) ->
    case proplists:get_value(Option, Rules, unknown) of
        unknown ->
            [aere_theme:error("Unknown setting: "), aere_theme:setting(atom_to_list(Option))];
        Scheme ->
            [ aere_theme:error("Bad setting format\n")
            , aere_theme:output("USAGE: ")
            , aere_theme:setting(atom_to_list(Option) ++ " ")
            , aere_theme:setting_arg(format_option_scheme(Scheme))
            ]
    end.

format_option_scheme(integer) -> "INTEGER";
format_option_scheme(boolean) -> "BOOLEAN";
format_option_scheme({atom, Ats}) -> string:join(lists:map(fun atom_to_list/1, Ats), "|");
format_option_scheme({valid, Kind, _, Expl}) ->
    format_option_scheme(Kind) ++ "(" ++ Expl ++ ")".

-spec bye() -> msg().
bye() -> aere_theme:output("bye!").
