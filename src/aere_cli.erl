-module(aere_cli).

-export([run/0, run/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").


join(R) ->
    R ! {join, self()}.
leave(R) ->
    R ! {leave, self()}.
query(R, S, Msg) ->
    R ! {input, S, Msg}.
answer(R, Msg) ->
    R ! {answer, Msg}.

get_resp(R) ->
    receive
        {response, R, Res = #repl_response{}} -> Res;
        finito -> finito
    end.

run() ->
    R = spawn(aerepl@localhost, aerepl, start, []),
    run(R),
    leave(R).
run(R) ->
    join(R),
    Resp = get_resp(R),
    {success, InitState} = Resp#repl_response.status,
    WithColors = InitState#repl_state{options = InitState#repl_state.options#options{colors = emph}},

    {ok, CWD} = file:get_cwd(),
    WithCWD = WithColors#repl_state{cwd=CWD},
    print_response(WithCWD, Resp#repl_response{status = {success, WithCWD}}),
    loop(R, WithCWD).


send_input(R, query, S) ->
    Inp = aere_parse:get_input(fun io:get_line/1),
    query(R, S, Inp);
send_input(R, answer, _) ->
    Inp = case io:get_line("? ") of
              eof -> "eof";
              Str -> Str
          end,
    Ans = string:trim(Inp, both, aere_parse:whitespaces()),
    answer(R, Ans).

print_response(State, #repl_response
               { output = Out
               , warnings = Warnings
               , status = Status
               }) ->
    UsedState = case Status of
                    {success, State1} -> State1;
                    _ -> State
                end,
    [aere_repl:print_msg( UsedState
                        , [aere_color:yellow("Warning: "), W, "\n"])
     || W <- Warnings],
    case Status of
        error -> aere_repl:print_msg(UsedState, aere_color:red("ERROR"));
        _ -> ok
    end,
    aere_repl:print_msg(UsedState, Out).

loop(R, State) ->
    loop(R, query, State).
loop(R, Type, State) ->
    send_input(R, Type, State),
    case get_resp(R) of
        Resp = #repl_response{} ->
            print_response(State, Resp),
            case Resp#repl_response.status of
                {success, State1} ->
                    loop(R, State1);
                ask ->
                    loop(R, answer, State);
                error ->
                    loop(R, State);
                internal_error ->
                    loop(R, State);
                finito ->
                    finito
            end;
        finito -> finito
    end.
