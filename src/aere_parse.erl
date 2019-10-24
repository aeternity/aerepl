-module(aere_parse).

-export([get_input/0, dispatch/1, whitespaces/0]).

-spec whitespaces() -> list(char()).
whitespaces() ->
    [$\n, $ , $\t, $ ].

-spec commands() -> list(aere_repl:command()).
commands() ->
    [ quit, type, eval, include, reinclude, cd, pwd
    , uninclude, set, load, deploy, 'let', def, unlet, undef, undeploy, rm, list].

-spec default_command() -> aere_repl:command().
default_command() ->
    eval.

-spec dispatch(string()) -> {ok, {aere_repl:command(), string()}}
                          | skip
                          | {error, {no_such_command, string()}
                                  | {ambiguous_prefix, list(aere_repl:command())}}.
dispatch(Input) ->
    case Input of
        [] ->
            skip;
        ":" ->
            skip;
        [$:|CommandAndArg] ->
            [Command | _] = string:tokens(CommandAndArg, whitespaces()),
            Arg = string:trim(CommandAndArg -- Command, leading, whitespaces()),
            CommandStrs = [atom_to_list(C) || C <- commands()],
            case lists:filter(fun(C) -> lists:prefix(Command, C) end, CommandStrs) of
                [C] -> {ok, {list_to_existing_atom(C), Arg}};
                [] -> {error, {no_such_command, Command}};
                More -> case lists:member(Command, CommandStrs) of
                            true -> {ok, {list_to_existing_atom(Command), Arg}};
                            false -> {error, {ambiguous_prefix, [list_to_existing_atom(C) || C <- More]}}
                        end
            end;
        _ -> {ok, {default_command(), Input}}
    end.

-spec get_input() -> string().
get_input() ->
    Line = io:get_line("AESO> "),
    Inp = case Line of
              ":{\n" ->
                  multiline_input();
              "" ->
                  "";
              eof -> % that's dirty
                  ":quit";
              _ ->
                  lists:flatten(string:replace(Line, ";", "\n", all))
          end,
    string:trim(Inp, both, whitespaces()).

-spec multiline_input() -> string().
multiline_input() ->
    multiline_input([]).
multiline_input(Acc) ->
    Line = io:get_line("| "),
    case Line of
        ":}\n" -> lists:flatten(lists:reverse(Acc));
        _ -> multiline_input([Line|Acc])
    end.

