-module(aere_parse).

-export([ parse/1, words/1, commands/0, resolve_command/1 ]).

-type parse_result() :: {atom(), string() | [string()]}
                      | no_return().

-type command_scheme() :: none
                        | consume
                        | many_args
                        | {n_args, non_neg_integer()}
                        | {min_args, non_neg_integer()}
                        | {max_args, non_neg_integer()}.

-type command_spec() :: {string(), {[string()], command_scheme(), string(), string()}}.

-spec commands() -> [command_spec()].
commands() ->
    [ {"reset",
       {[], none, "",
        ["Resets the REPL to the initial state."]
       }}
    , {"quit",
       {["q"], none, "",
        ["Terminates the REPL"]
       }}
    , {"type",
       {["t"], consume, "SOPHIA_EXPR",
        ["Typechecks a Sophia expression"]
       }}
    , {"eval",
       {[], consume, "SOPHIA_EXPR|SOPHIA_DECL",
        [ "Evaluates a Sophia expression and prints according to the print_format setting."
        , "Consumes Sophia declarations and extends REPL context accordingly."]
       }}
    , {"load",
       {["l"], {min_args, 1}, "FILENAMES",
        [ "Loads files into the REPL. Adds the last file to the scope."
        , "Cleans user variables, functions, types and contracts."
        , "Unloads previously loaded files."]
       }}
    , {"reload",
       {["r"], many_args, "[FILENAMES]",
        [ "Reloads given files preserving the included scope."
        , "Cleans user variables, functions, types and contracts."]
       }}
    , {"set",
       {["s"], {min_args, 1}, "SETTING [SETTING_ARGS]",
        [ "Configures REPL environment and behavior. "
        , "Possible usages:"] ++
            [ "- :set " ++ atom_to_list(Opt) ++ " " ++ aere_options:format_option_scheme(Scheme)
              || {Opt, Scheme} <- aere_options:option_parse_rules()
            ]
       }}
    , {"state",
       {["st"], consume, "SOPHIA_EXPR",
        [ "Changes the in-REPL state value. Expects Sophia expression, not type."
        , "This is used when the new value is of a different type and therefore"
        , "cannot be adjusted using the put function."
        , "Cleans user variables and functions."]
       }}
    , {"event",
       {["ev"], consume, "SOPHIA_TYPE",
        [ "Changes the in-REPL event type. Expects a Sophia variant type."
        , "Cleans user variables, functions, typedefs, type scopes, and older events."]
       }}
    , {"print",
       {["p"], {n_args, 1}, "WHAT",
        [ "Prints REPL state. The argument determines what component is to be printed."
        , "Possible componens:"
        , "- vars: displays user-defined variables"
        , "- types: displays user-defined types"
        , "- options: displays the current configuration"
        , "- files: displays loaded files"
        , "- includes: displays files included in the scope"]
       }}
    , {"help",
       {["h"], {max_args, 1}, "[COMMAND]",
        [ "Displays help about the command if the command is defined."
        , "Otherwise displays general help."]
       }}
    , {"disas",
       {["d"], {n_args, 1}, "FUNCTION_NAME|CONTRACT.FUNCTION_NAME",
       [ "Prints FATE assembly of a function. Can be an in-REPL function"
       , "or a function defined in a loaded or created contract or namespace."]
       }}
    ].

resolve_command(Cmd) ->
    case lists:search(
           fun({C, {Aliases, _, _, _}}) -> (Cmd == C) orelse lists:member(Cmd, Aliases) end,
           commands()) of
        {value, {C, V}} -> {list_to_atom(C), V};
        false -> undefined
    end.

%% Parse an input string. This function is called on strings entered by the user in the repl
-spec parse(string()) -> parse_result().
parse(Input) ->
    case string:trim(Input) of
        []  -> {skip, []};
        ":" -> {skip, []};
        [$:|CommandAndArgs] ->
            {CommandStr, ArgStr} = split_first_word(CommandAndArgs),
            case resolve_command(CommandStr) of
                undefined -> throw({repl_error, aere_msg:no_such_command(CommandStr)});
                {Command, {_Aliases, ArgScheme, ArgDoc, _Doc}} ->
                    case parse_args(ArgScheme, ArgStr) of
                        {ok, Args} -> {Command, Args};
                        error -> throw({repl_error, aere_msg:bad_command_args(CommandStr, ArgDoc)})
                    end
            end;
        _ ->
            %% Eval is the default command (i.e. 1 + 1 is just :eval 1 + 1)
            {eval, Input}
    end.

-spec parse_args(command_scheme(), string()) -> {ok, string() | [string()]} | error.
parse_args(consume, Str) ->
    {ok, Str};
parse_args(none, Str) ->
    case string:trim(Str) of
        "" -> {ok, []};
        _ -> error
    end;
parse_args(many_args, Str) ->
    {ok, words(Str)};
parse_args({n_args, N}, Str) ->
    Words = words(Str),
    case length(Words) == N of
        true -> {ok, Words};
        _ -> error
    end;
parse_args({min_args, N}, Str) ->
    Words = words(Str),
    case length(Words) >= N of
        true -> {ok, Words};
        _ -> error
    end;
parse_args({max_args, N}, Str) ->
    Words = words(Str),
    case length(Words) =< N of
        true -> {ok, Words};
        _ -> error
    end.

words(String) ->
    string:lexemes(String, unicode_util:whitespace()).

split_first_word(String) ->
    Word = string:nth_lexeme(String, 1, unicode_util:whitespace()),
    Rest = lists:nthtail(length(Word), String),
    {Word, Rest}.
