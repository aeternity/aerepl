-module(aere_msg).

-include("aere_macros.hrl").

%% Misc
-export(
   [ output/1
   , error/1
   , internal/1
   , internal/2
   ]).

%% Errors
-export(
   [ abort/2
   , no_such_command/1
   , bad_command_args/2
   , file_not_loaded/1
   , files_load_error/1
   , state_typedef/0
   , event_typedef/0
   , chain_not_ready/0
   , chain_not_running/0
   , locked_option/0
   , list_unknown/1
   , bad_fun_ref/0
   , contract_not_found/0
   , function_not_found_in/1
   , not_at_breakpoint/0
   , breakpoint_out_of_range/1
   , breakpoint_wrong_location/2
   , breakpoint_file_not_loaded/1
   , filesystem_not_cached/0
   , contract_exec_ended/0
   , undefined_variable/1
   , unsupported_eval/1
   ]).

%% Messages
-export(
   [ banner/0
   , version_info/1
   , eval/2
   , used_gas/1
   , stacktrace/1
   , type/1
   , lookup/2
   , fate/1
   , location/2
   , list_vars/1
   , list_types/1
   , list_options/1
   , list_loaded_files/1
   , list_includes/1
   , list_breakpoints/1
   , help/0, help/1
   , option_usage/1
   , bye/0
   , debug_vars/1
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
    VersionInfo = version_info(aere_version:version_info()),

    [SophiaThemed, InteractiveThemed, VersionInfo].

version_info(
  #{ protocol_version := Protocol
   , repl_version := REPL
   , compiler_version := Sophia
   , node_version := Node
   }
 ) ->

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
     || {I,
         #{ contract := Con
          , function := Fun
          , file := File
          , line := Line
          }
        } <- lists:zip(lists:seq(1, length(Stack)), Stack)
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
    InclOpts = maps:keys(aere_repl_state:init_options()),
    ExclOpts = [theme],
    OptsS =
        [ atom_to_list(Opt) ++ " = " ++ lists:flatten(io_lib:format("~p", [Val]))
         || {Opt, Val} <- maps:to_list(Opts),
            not lists:member(Opt, ExclOpts),
            lists:member(Opt, InclOpts)
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

-spec contract_exec_ended() -> msg().
contract_exec_ended() ->
    aere_theme:output("Contract execution was either aborted or exited.").

-spec breakpoint_out_of_range(pos_integer()) -> msg().
breakpoint_out_of_range(Index) ->
    aere_theme:error(io_lib:format("A breakpoint with the index ~p does not exist", [Index])).

-spec breakpoint_wrong_location(string(), pos_integer()) -> msg().
breakpoint_wrong_location(File, Line) ->
    aere_theme:error(io_lib:format("A breakpoint in the file `~s` at line ~p does not exist", [File, Line])).

-spec breakpoint_file_not_loaded(string()) -> msg().
breakpoint_file_not_loaded(FileName) ->
    aere_theme:error(io_lib:format("Cannot add a breakpoint because the file `~s` is not loaded in the repl", [FileName])).

-spec filesystem_not_cached() -> msg().
filesystem_not_cached() ->
    aere_theme:error("This operation is allowed only when the filesystem is in cached mode.").

-spec undefined_variable(string()) -> msg().
undefined_variable(VarName) ->
    aere_theme:error(io_lib:format("Undefined variable `~s`", [VarName])).

-spec unsupported_eval(aere_sophia:parse_result()) -> msg().
unsupported_eval([{letfun, Ann, FName, Args, _RetT, _Body}]) ->
    Wildcard = {id, [{origin, system}], "_"},
    Hole = {id, [{origin, system}], "???"},
    LamArgs = [{arg, Ann, Wildcard, Hole} || _A <- Args],
    Lam = {lam, Ann, LamArgs, Wildcard},
    LetLam = {letval, Ann, FName, Lam},

    ErrMsg = aere_theme:error("Top-level function definitions are not supported.\n"),
    HintMsg = aere_theme:info("Hint: maybe a non-recursive lambda would work:\n"),
    ExampleMsg = aere_theme:info(prettypr:format(aeso_pretty:decl(LetLam))),

    [ErrMsg, HintMsg, ExampleMsg];
unsupported_eval([_, _ | _]) ->
    aere_theme:error("Multiple queries are not supported.");
unsupported_eval(_) ->
    aere_theme:error("Unsupported query").

-spec bye() -> msg().
bye() -> aere_theme:output("bye!").

-spec help() -> msg().
help() ->
    Help =
        [ "Type a Sophia expression to evaluate it. Commands supported by the REPL:"
        ] ++
        [ "- :" ++ atom_to_list(Command)
          || {Command, _} <- aere_parse:commands()
        ] ++
        ["Type `:help COMMAND` to learn about the given command"],

    aere_theme:info(string:join(Help, "\n")).

-spec help(aere_parse:command_spec()) -> msg().
help(Spec) ->
    {Cmd, {Aliases, _, ArgDoc, Doc}} = Spec,
    AliasesStr = string:join([":" ++ atom_to_list(A) || A <- Aliases], ", "),
    command_usage(Cmd, ArgDoc) ++
        [aere_theme:info("\nALIASES: "), aere_theme:command(AliasesStr)] ++
        [aere_theme:output("\n\n")] ++
        [aere_theme:info(string:join(Doc, "\n"))]
        .

-spec type(aeso_syntax:type()) -> msg().
type(Type) ->
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    TypeStrClean = re:replace(TypeStr, ?TYPE_CONTAINER_RE, "", [global, {return, list}]),
    aere_msg:output(TypeStrClean).

-spec eval(Eval, aere_repl_state:options()) -> msg() when
      Eval :: ok | {msg, msg()} | aere_fate:eval_debug_result().
eval(ok, _) ->
    [];
eval({msg, Msg}, _) ->
    Msg;
eval({ok, #{result := Res, type := Type, used_gas := UsedGas}}, Opts) ->
    #{ display_gas  := DisplayGas,
       print_unit   := PrintUnit,
       print_type   := PrintType
     } = Opts,
    TypeStr = aeso_ast_infer_types:pp_type("", Type),
    PrintRes =
        case Type of
            {id, _, "unit"} -> PrintUnit;
            {tuple_t, _, []} -> PrintUnit;
            _ -> true
        end,

    [ [aere_theme:output(Res) || PrintRes]
    , [aere_theme:info(" : " ++ TypeStr) || PrintRes andalso PrintType]
    , ["\n" || PrintRes andalso DisplayGas]
    , [used_gas(UsedGas) || DisplayGas]
    ];
eval(break, _Opts) ->
    aere_msg:output("Break");
eval({revert, #{err_msg := ErrMsg, stacktrace := Stacktrace}}, _Opts) ->
    abort(ErrMsg, Stacktrace).

-spec lookup(string(), list(term())) -> msg().
lookup(What, Data) ->
    case What of
        "vars"        -> list_vars(Data);
        %% "funs"        -> aere_msg:list_funs(Data),
        "types"       -> list_types(Data);
        "options"     -> list_options(Data);
        "files"       -> list_loaded_files(Data);
        "includes"    -> list_includes(Data);
        "breakpoints" -> list_breakpoints(Data)
    end.

-spec fate(term()) -> msg().
fate(Fate) ->
    FateStr = lists:flatten(aeb_fate_asm:pp(Fate)),
    aere_msg:output(FateStr).

-spec location(Location, aere_repl_state:state()) -> msg() when
      Location :: aere_debugger:source_location().
location(#{file := FileName, line := CurrentLine}, RS) ->
    #{ loc_backwards := LocBackwards
     , loc_forwards := LocForwards
     } = aere_repl_state:options(RS),
    File     = aere_files:read_file(FileName, RS),
    Lines    = string:split(File, "\n", all),
    Enumerate     = fun(List) -> lists:zip(lists:seq(1, length(List)), List) end,
    SelectLines   = [ {Idx, Line}
                      || {Idx, Line} <- Enumerate(Lines),
                         Idx > CurrentLine - LocBackwards,
                         Idx < CurrentLine + LocForwards
                    ],

    LineSign =
        fun(Id) when Id == CurrentLine -> ">";
           (_)                         -> "|"
        end,
    MaxDigits     = length(integer_to_list(length(Lines))),
    FormatLineNum = fun(Num) -> string:right(integer_to_list(Num), MaxDigits) end,
    FormatLine    = fun(N, Ln) -> [LineSign(N), " ", FormatLineNum(N), " ", Ln] end,
    NewLines = [FormatLine(Idx, Line) || {Idx, Line} <- SelectLines],
    aere_msg:output(lists:join("\n", NewLines)).

-spec debug_vars(list({string(), string()})) -> msg().
debug_vars(Vars) ->
    Dump = [io_lib:format("~s:\t~s", [K, V]) || {K, V} <- Vars],

    output(lists:join("\n", Dump)).
