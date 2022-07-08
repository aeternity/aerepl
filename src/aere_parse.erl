-module(aere_parse).

-export([get_input/1, eval_from_file/1, dispatch/1, split_input/1]).

-spec commands() -> list(aere_repl:command()).
commands() ->
    [ quit, type, eval, include, reinclude, cd, pwd, ls, continue
    , uninclude, set, load, deploy, rm, reset, '_names'].

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
            [Command | _] = string:tokens(CommandAndArg, unicode_util:whitespace()),
            Arg = string:trim(CommandAndArg -- Command, leading, unicode_util:whitespace()),
            CommandStrs = [atom_to_list(C) || C <- commands()],
            case lists:filter(fun(C) -> lists:prefix(Command, C) end, CommandStrs) of
                [C] -> {ok, {list_to_existing_atom(C), Arg}};
                [] -> {error, {no_such_command, Command}};
                More -> case lists:member(Command, CommandStrs) of
                            true -> {ok, {list_to_existing_atom(Command), Arg}};
                            false -> {error, {ambiguous_prefix, [list_to_existing_atom(C) || C <- More]}}
                        end
            end;
        _ -> {ok, {eval, Input}}
    end.

eval_from_file(File) ->
    MC = file:read_file(File),
    C = case MC of
            {error, Reason} ->
                throw(aere_error:file_error(File, Reason));
            {ok, F} -> binary_to_list(F)
    end,
    [dispatch(I) || I <- split_input(C)].

split_input(C) ->
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
                        [get_input(FileEater)| R()]
                end
        end,
    Reads().


get_input(Provider) ->
    Line = case Provider("AESO> ") of
               eof -> ":quit"; % that's dirty
               Other -> Other
           end,
    Inp = case string:trim(Line, both, unicode_util:whitespace()) of
              ":{" -> multiline_input(Provider);
              "" -> "";
              _ -> lists:flatten(string:replace(Line, ";", "\n", all))
          end,
    string:trim(Inp, both, unicode_util:whitespace()).

multiline_input(Provider) ->
    multiline_input(Provider, []).
multiline_input(Provider, Acc) ->
    Line = Provider("| "),
    case string:trim(Line, both, unicode_util:whitespace()) of
        ":}" -> lists:flatten(lists:reverse(Acc));
        _ -> multiline_input(Provider, [Line|Acc])
    end.
