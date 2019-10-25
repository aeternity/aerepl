-module(aere_parse).

-export([get_input/1, eval_from_file/1, dispatch/1, whitespaces/0]).

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


eval_from_file(File) ->
    MC = file:read_file(File),
    C = case MC of
            {error, Reason} ->
                throw({ error
                      , case Reason of
                            enoent -> "No such file " ++ File;
                            eaccess -> "Permission denied";
                            eisdir -> File ++ " is a directory";
                            enotdir -> "Invalid path";
                            enomem -> File ++ " is too big";
                            _ -> "Unknown error"
                        end
                      });
            {ok, F} -> binary_to_list(F)
    end,
    FileSpitterProcess =
        fun R(S) ->
                receive
                    die -> ok;
                    {is_eof, Back} -> Back ! (S == []),
                                      R(S);
                    {read, Back} ->
                        case S of
                            [] -> Back ! eof,
                                  R([]);
                            [H|T] -> Back ! H,
                                     R(T)
                        end
                end
        end,
    Lines = [L ++ "\n" || L <- string:tokens(C, "\n")],
    FileSpitter = spawn(fun () -> FileSpitterProcess(Lines) end),
    FileEater =
        fun(_) ->
                FileSpitter ! {read, self()},
                receive S -> S end
        end,
    Reads =
        fun R() ->
                FileSpitter ! {is_eof, self()},
                receive
                    true ->
                        FileSpitter ! die,
                        [];
                    false ->
                        X = get_input(FileEater),
                        [dispatch(X) | R()]
                end
        end,
    Reads().


get_input(Provider) ->
    Line = Provider("AESO> "),
    Inp = case string:trim(Line, both, whitespaces()) of
              ":{" -> multiline_input(Provider);
              "" -> "";
              eof -> ":quit"; % that's dirty
              _ -> lists:flatten(string:replace(Line, ";", "\n", all))
          end,
    string:trim(Inp, both, whitespaces()).


multiline_input(Provider) ->
    multiline_input(Provider, []).
multiline_input(Provider, Acc) ->
    Line = Provider("| "),
    case string:trim(Line, both, whitespaces()) of
        ":}" -> lists:flatten(lists:reverse(Acc));
        eof -> ":quit";
        _ -> multiline_input(Provider, [Line|Acc])
    end.

