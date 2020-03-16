-module(aere_cli).

-export([run/0, run/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").


run() ->
    R = spawn(aerepl, start, []),
    run(R),
    R ! finito.
run(R) ->
    aerepl:join(R),
    MyR = receive {your_repl, Pid} -> Pid end,
    Resp = aerepl:get(MyR),
    {success, InitState} = Resp#repl_response.status,
    print_response(InitState, Resp),
    loop(MyR, InitState).


send_input(R, query, S) ->
    Inp = aere_parse:get_input(fun io:get_line/1),
    aerepl:query(R, S, Inp);
send_input(R, answer, _) ->
    Ans = string:trim(io:get_line("? "), both, aere_parse:whitespaces()),
    aerepl:answer(R, Ans).

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
    aere_repl:print_msg(UsedState, Out).

loop(R, State) ->
    loop(R, query, State).
loop(R, Type, State) ->
    send_input(R, Type, State),
    case aerepl:get(R) of
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
                finito -> finito
            end
    end.
