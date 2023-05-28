-module(aere_parse).

-export([ parse/1, commands/0, resolve_command/1 ]).

-type parse_result() :: {atom(), string() | [string()]}
                      | no_return().

-type arg_type() :: integer | atom | string.

-type arg() :: integer() | atom() | string().

-type arg_scheme() :: {many, arg_type()}
                    | {some, arg_type()}
                    | {optional, arg_type()}
                    | {required, arg_type()}.

-type command_scheme() :: [arg_scheme()] | consume.

-type command_spec() :: {atom(), {[atom()], command_scheme(), string(), string()}}.


-spec commands() -> [command_spec()].

commands() ->
    [ {reset,
       {[], [], "",
        ["Resets the REPL to the initial state."]
       }}
    , {quit,
       {[q], [], "",
        ["Terminates the REPL"]
       }}
    , {type,
       {[t], consume, "SOPHIA_EXPR",
        ["Typechecks a Sophia expression"]
       }}
    , {eval,
       {[], consume, "SOPHIA_EXPR|SOPHIA_DECL",
        [ "Evaluates a Sophia expression and prints according to the print_format setting."
        , "Consumes Sophia declarations and extends REPL context accordingly."]
       }}
    , {load,
       {[l], [{some, string}], "FILENAMES",
        [ "Loads files into the REPL. Adds the last file to the scope."
        , "Cleans user variables, functions, types and contracts."
        , "Unloads previously loaded files."]
       }}
    , {reload,
       {[r], [], "",
        [ "Reloads loaded files preserving the included scope."
        , "Cleans user variables, functions, types and contracts."]
       }}
    , {set,
       {[s], [{required, atom}, {some, string}], "SETTING [SETTING_ARGS]",
        [ "Configures REPL environment and behavior. "
        , "Possible usages:"] ++
            [ "- :set " ++ atom_to_list(Opt) ++ " " ++ aere_options:format_option_scheme(Scheme)
              || {Opt, Scheme} <- aere_options:option_parse_rules()
            ]
       }}
    , {state,
       {[st], consume, "SOPHIA_EXPR",
        [ "Changes the in-REPL state value. Expects Sophia expression, not type."
        , "This is used when the new value is of a different type and therefore"
        , "cannot be adjusted using the put function."
        , "Cleans user variables and functions."]
       }}
    , {print,
       {[p], [{required, string}], "WHAT",
        [ "Prints REPL state. The argument determines what component is to be printed."
        , "Possible componens:"
        , "- vars: displays user-defined variables"
        , "- types: displays user-defined types"
        , "- options: displays the current configuration"
        , "- files: displays loaded files"
        , "- includes: displays files included in the scope"
        , "- breakpoints: displays all breakpoints set in the debugger" ]
       }}
    , {help,
       {[h], [{optional, string}], "[COMMAND]",
        [ "Displays help about the command if the command is defined."
        , "Otherwise displays general help."]
       }}
    , {disas,
       {[d], [{required, string}], "FUNCTION_NAME|CONTRACT.FUNCTION_NAME",
       [ "Prints FATE assembly of a function. Can be an in-REPL function"
       , "or a function defined in a loaded or created contract or namespace."]
       }}
    , {break,
        {[b], [{required, string}, {required, integer}], "FILE_NAME LINE_NUM",
        [ "Set a breakpoint in the specified file and line." ]}}
    , {delete_break,
        {[db], [{required, integer}], "BREAKPOINT_INDEX",
        [ "Delete a breakpoint." ]}}
    , {stepin,
        {[si], [], "",
        [ "Resume execution after a breakpoint is hit, until the next line." ]}}
    , {stepover,
        {[sv], [], "",
        [ "Resume execution after a breakpoint is hit, until the next line, skipping over function calls." ]}}
    , {stepout,
        {[so], [], "",
        [ "Resume execution after a breakpoint is hit, until the current function is finished" ]}}
    , {continue,
        {[c], [], "",
        [ "Resume execution after a breakpoint is hit, until the next breakpoint." ]}}
    , {print_var,
        {[pv], [{required, string}], "VAR_NAME",
        [ "Print the value of a variable when a breakpoint is hit." ]}}
    , {location,
        {[loc], [], "",
        [ "Print the Sophia source file with a mark on the currently executing line." ]}}
    , {stacktrace,
        {[bt], [], "",
        [ "Print the stacktrace at the current point of execution." ]}}
    ].


-spec resolve_command(atom()) -> command_spec() | undefined.

resolve_command(Cmd) ->
    case resolve_command_by_name(Cmd, commands()) of
        undefined -> resolve_command_by_alias(Cmd, commands());
        Spec      -> Spec
    end.


-spec resolve_command_by_name(atom(), [command_spec()]) -> command_spec() | undefined.

resolve_command_by_name(_, []) ->
    undefined;
resolve_command_by_name(Name, [Spec = {Command, _} | Rest]) ->
    case Name == Command of
        true  -> Spec;
        false -> resolve_command_by_name(Name, Rest)
    end.


-spec resolve_command_by_alias(atom(), [command_spec()]) -> command_spec() | undefined.

resolve_command_by_alias(_, []) ->
    undefined;
resolve_command_by_alias(Alias, [Spec = {_, {Aliases, _, _, _}} | Rest]) ->
    case lists:member(Alias, Aliases) of
        true  -> Spec;
        false -> resolve_command_by_alias(Alias, Rest)
    end.


-spec parse(string()) -> parse_result().
%% @doc
%% Parse an input string. This function is called on strings entered by the user in the repl

parse(Input) ->
    case string:trim(Input) of
        [] ->
            skip;
        ":" ->
            skip;
        [$:|CommandAndArgs] ->
            parse_command(CommandAndArgs);
        _ ->
            %% Eval is the default command (i.e. 1 + 1 is just :eval 1 + 1)
            {eval, Input}
    end.


parse_command(CommandAndArgs) ->
    {Cmd, ArgStr} = split_command(CommandAndArgs),
    case resolve_command(Cmd) of
        {Command, {_, Scheme, ArgDoc, _}} ->
            try parse_command_scheme(Scheme, ArgStr) of
                {ok, []}   -> Command;
                {ok, Args} -> list_to_tuple([Command | Args]);
                error      -> {error, aere_msg:bad_command_args(Command, ArgDoc)}
            catch
                {parse_error, {bad_integer, _}} ->
                    {error, aere_msg:bad_command_args(Command, ArgDoc)}
            end;
        undefined ->
            {error, aere_msg:no_such_command(Cmd)}
    end.


-spec parse_command_scheme(command_scheme(), string()) -> {ok, [arg() | [arg()]]} | error.

parse_command_scheme(consume, Str) ->
    {ok, [Str]};
parse_command_scheme(Scheme, Str) ->
    parse_args(Scheme, words(Str)).


-spec parse_args(Args, Words) -> {ok, [arg() | [arg()]]} | error
    when Args  :: [arg_scheme()],
         Words :: [string()].
parse_args([], []) ->
    {ok, []};
parse_args([], _) ->
    error;
parse_args([{some, _Type}], []) ->
    error;
parse_args([{some, Type}], Words) ->
    {ok, [ [apply_type(Word, Type) || Word <- Words] ]};
parse_args([{many, Type}], Words) ->
    {ok, [ [apply_type(Word, Type) || Word <- Words] ]};
parse_args([{optional, _Type}], []) ->
    {ok, []};
parse_args([{optional, Type}], [Word]) ->
    {ok, [apply_type(Word, Type)]};
parse_args([{optional, _Type}], _) ->
    error;
parse_args([{required, Type} | RestArgs], [Word | RestWords]) ->
    case parse_args(RestArgs, RestWords) of
        {ok, Rest} -> {ok, [apply_type(Word, Type) | Rest]};
        error      -> error
    end;
parse_args(_, _) ->
    error.


-spec apply_type(string(), arg_type()) -> arg() | no_return().

apply_type(X, integer) ->
    try
        list_to_integer(X)
    catch
        error:badarg ->
            throw({parse_error, {bad_integer, X}})
    end;

apply_type(X, atom) ->
    list_to_atom(X);

apply_type(X, string) ->
    X;

apply_type(_, T) ->
    throw({parse_error, {unknown_arg_type, T}}).


-spec words(string()) -> [string()].

words(String) ->
    string:lexemes(String, unicode_util:whitespace()).


-spec split_command(string()) -> {atom(), string()}.

split_command(String) ->
    Word = string:nth_lexeme(String, 1, unicode_util:whitespace()),
    Rest = lists:nthtail(length(Word), String),
    {list_to_atom(Word), Rest}.
