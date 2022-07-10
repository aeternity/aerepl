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
        , bye/0
        ]).

-spec banner() -> aere_theme:renderable().
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

-spec output(string()) -> aere_theme:renderable().
output(Msg) -> aere_theme:output(Msg).

-spec output_with_gas(string(), integer()) -> aere_theme:renderable().
output_with_gas(Msg, UsedGas) ->
    GasMsg = io_lib:format("\nUSED GAS: ~p", [UsedGas]),
    [aere_theme:output(Msg), aere_theme:info(GasMsg)].

-spec error(string()) -> aere_theme:renderable().
error(Msg) -> aere_theme:error(Msg).

-spec abort(string()) -> aere_theme:renderable().
abort(Msg) -> [aere_theme:error("ABORT: "), aere_theme:info(Msg)].

-spec internal(term(), erlang:stacktrace()) -> aere_theme:renderable().
internal(Error, Stacktrace) ->
    [ aere_theme:output("Command failed:\n")
    , aere_theme:error(lists:flatten(io_lib:format("~p", [Error])))
    , aere_theme:output("\n")
    , case Stacktrace of
          [] -> aere_theme:output("<no stacktrace>");
          _  -> aere_theme:output("Stacktrace:\n" ++ io_lib:format("~p", [Stacktrace]))
      end
    ].

-spec internal(term()) -> aere_theme:renderable().
internal(Error) ->
    internal(Error, []).

-spec no_such_command(aere_repl:command()) -> aere_theme:renderable().
no_such_command(Command) ->
    [ aere_theme:output("No such command ")
    , aere_theme:command(io_lib:format("~p", [Command]))
    , aere_theme:output("\n")
    ].

-spec file_not_loaded(FileName) -> aere_theme:renderable() when
      FileName :: string().
file_not_loaded(File) ->
    [aere_theme:error("File not loaded: "), aere_theme:file(File)].

-spec files_load_error([{FileName, Reason}]) -> aere_theme:renderable() when
      FileName :: string(),
      Reason :: string().
files_load_error(Failed) ->
    FilesAndReasons = [ [ aere_theme:file(File)
                        , aere_theme:output(": ")
                        , aere_theme:output(Err)
                        ] || {File, Err} <- Failed ],
    FlatList = lists:flatten(lists:join(aere_theme:output("\n"), FilesAndReasons)),
    [aere_theme:error("Could not load files:\n") | FlatList].

-spec bye() -> aere_theme:renderable().
bye() -> aere_theme:output("bye!").
