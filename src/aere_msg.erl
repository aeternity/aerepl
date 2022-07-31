-module(aere_msg).

-export([ banner/0
        , output/1
        , used_gas/1
        , error/1
        , abort/1
        , internal/1
        , internal/2
        , no_such_command/1
        , file_not_loaded/1
        , files_load_error/1
        , chain_not_ready/0
        , set_nothing/0
        , option_usage/1
        , list_vars/1
        , list_types/1
        , list_options/1
        , list_loaded_files/1
        , list_includes/1
        , list_unknown/1
        , invalid_print/0
        , help/0, help/1
        , bye/0
        ]).

-include("aere_repl.hrl").

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
        "                 |_|  ",
    Interactive = "interactive\n\n",

    SophiaThemed = aere_theme:banner(Sophia),
    InteractiveThemed = aere_theme:banner_sub(Interactive),
    VersionInfo = version_info(),

    [SophiaThemed, InteractiveThemed, VersionInfo].

version_info() ->
    REPL = aere_version:repl_version(),
    Sophia = aere_version:compiler_version(),
    Protocol = integer_to_list(aere_version:protocol_version()),
    Node = aere_version:node_version(),

    aere_theme:info(
      string:join(
        [ "REPL version:     " ++ REPL
        , "Sophia version:   " ++ Sophia
        , "Node version:     " ++ Node
        , "Protocol version: " ++ Protocol
        ], "\n")
     ).

-spec trim(string()) -> string().
trim(S) ->
    string:trim(S, both).

-spec output(string()) -> msg().
output(Msg) -> aere_theme:output(trim(Msg)).

-spec used_gas(integer()) -> msg().
used_gas(UsedGas) ->
    GasMsg = io_lib:format("\nUSED GAS: ~p", [UsedGas]),
    aere_theme:info(GasMsg).

-spec error(string()) -> msg().
error(Msg) -> aere_theme:error(trim(Msg)).

-spec abort(string()) -> msg().
abort(Msg) -> [aere_theme:error("ABORT: "), aere_theme:info(trim(Msg))].

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

-spec option_usage(atom()) -> msg().
option_usage(Option) ->
    case proplists:get_value(Option, aere_options:option_parse_rules(), unknown) of
        unknown ->
            [aere_theme:error("Unknown setting: "), aere_theme:setting(atom_to_list(Option))];
        Scheme ->
            [ aere_theme:error("Bad setting format\n")
            , aere_theme:setting(atom_to_list(Option) ++ " ")
            , aere_theme:setting_arg(format_option_scheme(Scheme))
            ]
    end.

format_option_scheme(integer) -> "INTEGER";
format_option_scheme(boolean) -> "BOOLEAN";
format_option_scheme({atom, Ats}) -> string:join(lists:map(fun atom_to_list/1, Ats), "|");
format_option_scheme({valid, Kind, _, Expl}) ->
    format_option_scheme(Kind) ++ "(" ++ Expl ++ ")".

-spec chain_not_ready() -> msg().
chain_not_ready() ->
    [aere_theme:error("This operation runs only in chain-ready state.")].

-spec list_vars([string()]) -> msg().
list_vars(Vars) ->
    VarsS = [Var ++ " : " ++ aeso_ast_infer_types:pp_type("", Type)
             || {Var, Type, _} <- Vars],
    aere_theme:output(string:join(VarsS, "\n")).

-spec list_types([type_def()]) -> msg().
list_types(Types) ->
    TypesS = [ TName || {_, TName, _, _} <- Types],
    aere_theme:output(string:join(TypesS, "\n")).

-spec list_options(repl_options()) -> msg().
list_options(Opts) ->
    ExclOpts = [theme],
    OptsS =
        [ atom_to_list(Opt) ++ " = " ++ lists:flatten(io_lib:format("~p", [Val]))
         || {Opt, Val} <- maps:to_list(Opts),
            not lists:member(Opt, ExclOpts)
        ],
    aere_theme:output(string:join(OptsS, "\n")).

-spec list_loaded_files(#{string() => any()}) -> msg().
list_loaded_files(Files) ->
    aere_theme:output(string:join(maps:keys(Files), "\n")).

-spec list_includes([string()]) -> msg().
list_includes(Incs) ->
    aere_theme:output(string:join(Incs, "\n")).

-spec list_unknown([string()]) -> msg().
list_unknown(ToList) ->
    aere_theme:error("Possible items to print: " ++ string:join(ToList, ", ")).

-spec invalid_print() -> msg().
invalid_print() ->
    aere_theme:error("Argument error").

-spec bye() -> msg().
bye() -> aere_theme:output("bye!").

-spec help() -> msg().
help() ->
    aere_theme:info(string:join(h(general), "\n")).

-spec help(any()) -> msg().
help(What) ->
    aere_theme:info(string:join(h(What), "\n")).

%%% HELP STRINGS %%%

h("reset") ->
    [ "ARGS: none"
    , ""
    , "Restarts the REPL"
    ];
h("quit") ->
    [ "ARGS: none"
    , "ALIASES: :q"
    , ""
    , "Quits the REPL"
    ];
h("type") ->
    [ "ARGS: Sophia expression"
    , "ALIASES: :t"
    , ""
    , "Typechecks a Sophia expression"
    ];
h("eval") ->
    [ "ARGS: Sophia expression"
    , ""
    , "Evaluates a Sophia expression and prints according to the print_format setting."
    ];
h("load") ->
    [ "ARGS: [FILENAME]"
    , ""
    , "Loads files into the REPL. Adds the last file to the scope."
    , "Cleans user variables and functions. Unloads previously loaded files."
    ];
h("reload") ->
    [ "ARGS: [FILENAME]"
    , ""
    , "Reloads given files preserving the included scope."
    , "Cleans user variables and functions."
    ];
h("add") ->
    [ "ARGS: [FILENAME]"
    , ""
    , "Loads files into the REPL without unloading previously loaded files."
    , "Cleans user variables and functions."
    ];
h("set") ->
    Opts =
        [ "- :set " ++ atom_to_list(Opt) ++ " " ++ format_option_scheme(Scheme)
         || {Opt, Scheme} <- aere_options:option_parse_rules()
        ],
    [ "ARGS: SETTING [SETTING_ARGS]"
    , ""
    , "Configures REPL environment and behavior. "
    , "Possible usages:"
    ] ++ Opts;
h("state") ->
    [ "ARGS: Sophia expression"
    , ""
    , "Changes the in-REPL state value. Expects Sophia expression, not type."
    , "This is used when the new value is of a different type and therefore"
    , "cannot be adjusted using the put function."
    , "Cleans user variables and functions."
    ];
h("print") ->
    [ "ARGS: WHAT"
    , ""
    , "Prints REPL state. The argument determines what component is to be printed."
    , "Possible componens:"
    , "- vars: displays user-defined variables"
    , "- types: displays user-defined types"
    , "- options: displays the current configuration"
    , "- files: displays loaded files"
    , "- includes: displays files included in the scope"
    ];
h("help") ->
    [ "ARGS: COMMAND|none"
    , ""
    , "Displays help about the command if the command is defined."
    , "Otherwise displays general help."
    ];
h(_) ->
    [ "Type a Sophia expression to evaluate it. Commands supported by the REPL:"
    ] ++
    [ "- :" ++ atom_to_list(Command)
     || Command <- aere_parse:commands()
    ] ++
    ["Type `:help COMMAND` to learn about the given command"].
