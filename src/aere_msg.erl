-module(aere_msg).

-export([ banner/0
        , output/1
        , used_gas/1
        , error/1
        , abort/2
        , internal/1
        , internal/2
        , eval_result/2
        , no_such_command/1
        , bad_command_args/2
        , file_not_loaded/1
        , files_load_error/1
        , state_typedef/0
        , event_typedef/0
        , chain_not_ready/0
        , chain_not_running/0
        , locked_option/0
        , option_usage/1
        , list_vars/1
        , list_types/1
        , list_options/1
        , list_loaded_files/1
        , list_includes/1
        , list_breakpoints/1
        , list_unknown/1
        , bad_fun_ref/0
        , contract_not_found/0
        , function_not_found_in/1
        , not_at_breakpoint/0
        , stacktrace/1
        , breakpoint_out_of_range/1
        , undefined_variable/1
        , help/0, help/1
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
    GasMsg = io_lib:format("USED GAS: ~p", [UsedGas]),
    aere_theme:info(GasMsg).

-spec error(string()) -> msg().
error(Msg) -> aere_theme:error(trim(Msg)).

-spec abort(binary() | string(), [{term(), binary(), integer()}]) -> msg().
abort(Bin, Stack) when is_binary(Bin) -> abort(binary:bin_to_list(Bin), Stack);
abort(Msg, Stack) ->
    [ aere_theme:error("ABORT: ")
    , aere_theme:output(trim(Msg) ++ "\n")
    , stacktrace(Stack) ].

-spec stacktrace([{term(), binary(), integer()}]) -> msg().
stacktrace(Stack) ->
    NonEmpty =
        fun("") -> "aerepl";
           (F) when is_binary(F) -> binary_to_list(F);
           (F) -> F
        end,
    ContractStr =
        fun(C) when is_list(C) -> C;
           (C) when is_binary(C) -> binary:bin_to_list(aeser_api_encoder:encode(contract_pubkey, C))
        end,
    [ [ aere_theme:info(integer_to_list(I) ++ "    ")
      , aere_theme:info(ContractStr(Con)), aere_theme:info(":")
      , aere_theme:output(binary:bin_to_list(Fun)), aere_theme:info(":")
      , aere_theme:info(NonEmpty(File) ++ ":" ++ integer_to_list(Line) ++ "\n")
      ]
     || {I, {Con, Fun, File, Line}} <- lists:zip(lists:seq(1, length(Stack)), Stack)
    ].

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

-spec eval_result(string() | none, integer() | none) -> msg().
eval_result(none, none) ->
    [];
eval_result(Res, none) ->
    [aere_theme:output(Res)];
eval_result(none, Gas) ->
    [used_gas(Gas)];
eval_result(Res, Gas) ->
    [aere_theme:output(Res ++ "\n"), used_gas(Gas)].

-spec no_such_command(atom()) -> msg().
no_such_command(Command) ->
    [ aere_theme:output("No such command ")
    , aere_theme:command(io_lib:format("`~p`", [Command]))
    ].

-spec locked_option() -> msg().
locked_option() ->
    [aere_theme:error("This option is locked.")].

-spec command_usage(string() | atom(), string()) -> msg().
command_usage(Command, Doc) when is_atom(Command) ->
    command_usage(atom_to_list(Command), Doc);
command_usage(Command, Doc) ->
    [ aere_theme:info("USAGE: ")
    , aere_theme:command(":" ++ Command ++ " ")
    , aere_theme:output(Doc)
    ].

-spec bad_command_args(atom(), string()) -> msg().
bad_command_args(Command, Doc) ->
    [ aere_theme:error("Invalid parameters.\n")
    ] ++ command_usage(Command, Doc).

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

-spec state_typedef() -> msg().
state_typedef() ->
    [aere_theme:error("Cannot define a state type. Use :state to set the value of the state.")].

-spec event_typedef() -> msg().
event_typedef() ->
    [aere_theme:error("Cannot define an event type. Use :event to set the value of the event.")].

-spec option_usage(atom()) -> msg().
option_usage(Option) ->
    case proplists:get_value(Option, aere_options:option_parse_rules(), unknown) of
        unknown ->
            [aere_theme:error("Unknown setting: "), aere_theme:setting(atom_to_list(Option))];
        Scheme ->
            [ aere_theme:error("Bad setting format\n")
            , aere_theme:setting(atom_to_list(Option) ++ " ")
            , aere_theme:setting_arg(aere_options:format_option_scheme(Scheme))
            ]
    end.

-spec chain_not_ready() -> msg().
chain_not_ready() ->
    [aere_theme:error("This operation runs only in chain-ready state.")].

-spec chain_not_running() -> msg().
chain_not_running() ->
    [aere_theme:error("This operation runs only in chain-running state.")].

-spec list_vars([string()]) -> msg().
list_vars(Vars) ->
    VarsS = [Var ++ " : " ++ aeso_ast_infer_types:pp_type("", Type)
             || {Var, Type, _} <- Vars],
    aere_theme:output(string:join(VarsS, "\n")).

-spec list_types([aere_repl_state:type_def()]) -> msg().
list_types(Types) ->
    TypesS = [ TName ++
               case TArgs of
                   [] -> " : type";
                   _ -> " : (" ++ string:join(["type" || _ <- TArgs], ", ") ++ ") => type"
               end
	       || {_, TName, TArgs, _} <- Types],
    UniqTypesS = lists:usort(TypesS),
    aere_theme:output(string:join(UniqTypesS, "\n")).

-spec list_options(aere_repl_state:repl_options()) -> msg().
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

-spec list_breakpoints(aere_repl_state:breakpoints()) -> msg().
list_breakpoints(BPs) ->
    Enum = fun(List) -> lists:zip(lists:seq(1, length(List)), List) end,
    Msg  = "Breakpoint in the file '~s' at line ~p\n",
    [ [ aere_theme:info(io_lib:format("~p    ", [I]))
      , aere_theme:output(io_lib:format(Msg, [F, L]))
      ] || {I, {F, L}} <- Enum(BPs) ].

-spec list_unknown([string()]) -> msg().
list_unknown(ToList) ->
    aere_theme:error("Possible items to print: " ++ string:join(ToList, ", ")).

-spec bad_fun_ref() -> msg().
bad_fun_ref() ->
    aere_theme:error("Could not parse a function reference.").

-spec contract_not_found() -> msg().
contract_not_found() ->
    aere_theme:error("This contract is not deployed in the REPL context.").

-spec function_not_found_in(binary()) -> msg().
function_not_found_in(Name) ->
    aere_theme:error("This contract does not have a function of hash name " ++ binary:bin_to_list(Name) ++ ".").

-spec not_at_breakpoint() -> msg().
not_at_breakpoint() ->
    aere_theme:error("Not at breakpoint!").

-spec breakpoint_out_of_range(pos_integer()) -> msg().
breakpoint_out_of_range(Index) ->
    aere_theme:error(io_lib:format("A breakpoint with the index ~p does not exist", [Index])).

-spec undefined_variable(string()) -> msg().
undefined_variable(VarName) ->
    aere_theme:error(io_lib:format("Undefined variable `~s`", [VarName])).

-spec bye() -> msg().
bye() -> aere_theme:output("bye!").

-spec help() -> msg().
help() ->
    Help =
        [ "Type a Sophia expression to evaluate it. Commands supported by the REPL:"
        ] ++
        [ "- :" ++ Command
          || {Command, _} <- aere_parse:commands()
        ] ++
        ["Type `:help COMMAND` to learn about the given command"],

    aere_theme:info(string:join(Help, "\n")).

-spec help(any()) -> msg().
help(What) ->
    case aere_parse:resolve_command(list_to_atom(What)) of
        {Cmd, {Aliases, _, ArgDoc, Doc}} ->
            AliasesStr = string:join([":" ++ A || A <- Aliases], ", "),
            command_usage(Cmd, ArgDoc) ++
                [aere_theme:info("\nALIASES: "), aere_theme:command(AliasesStr)] ++
                [aere_theme:output("\n\n")] ++
                [aere_theme:info(string:join(Doc, "\n"))];
        _ ->
            help()
    end.
