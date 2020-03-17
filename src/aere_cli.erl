-module(aere_cli).

-export([run/0, run/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").


join(R) ->
    R ! {start, self()}.
query(R, S, Msg) ->
    R ! {input, self(), S, Msg}.
answer(R, Msg) ->
    R ! {answer, self(), Msg}.
goodbye(R) ->
    R ! finito.
get_resp(R) ->
    receive
        {response, R, Res = #repl_response{}} -> Res
    end.

run() ->
    R = spawn(aerepl@localhost, aerepl, start, []),
    run(R),
    R ! finito.
run(R) ->
    join(R),
    MyR = receive {your_repl, Pid} -> Pid end,
    Resp = get_resp(MyR),
    {success, InitState} = Resp#repl_response.status,
    print_response(InitState, Resp),
    loop(MyR, InitState).


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
                    goodbye(R),
                    finito
            end
    end.
