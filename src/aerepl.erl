-module(aerepl).

-export([ main/1, start/0, loop/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

main(_Args) ->
    start().

start() ->
    erlang:system_flag(backtrace_depth, 100),
    loop([]).

init_message() ->
    {response, self(), #repl_response
     { output = aere_repl:banner()
     , warnings = []
     , status = {success, aere_repl:init_state()}
     }}.

finito(Clients) ->
    [C ! finito || C <- Clients],
    finito.

loop(Clients) ->
    receive
        {join, Client} when is_pid(Client) ->
            ?IF(lists:member(Client, Clients),
                loop(Clients),
                begin Client ! init_message(),
                      loop([Client|Clients])
                end
               );
        {leave, Client} -> lists:delete(Client, Clients);
        {input, S = #repl_state{}, I} ->
                case process_input(Clients, S, I) of
                    continue -> loop(Clients);
                    finito -> finito(Clients)
                end,
                loop(Clients);
        finito -> finito(Clients)
    after 2000000000 -> finito(Clients)
    end.

process_input(Clients, State, Inp) ->
    case aere_repl:process_string(State, Inp) of
        Resp = #repl_response{} ->
            [C ! {response, self(), Resp}
             || C <- Clients
            ],
            continue;
        Q = #repl_question{} ->
            process_question(Clients, State, Inp, Q);
        _ -> continue
    end.

process_question(Clients, State, Inp, MyQ = #repl_question{}) ->
    [C ! {response, self(), aere_repl:question_to_response(MyQ)}
     || C <- Clients
    ],
    receive
        {answer, Ans} ->
            case aere_repl:answer(MyQ, Ans) of
                {retry, MyQ1} -> process_question(Clients, State, Inp, MyQ1);
                {accept, X} ->
                    {State1, Warns} = aere_repl:destroy_warnings(X),
                    [C !
                        { response
                        , self()
                        , #repl_response
                          { output = ""
                          , status = {success, State1}
                          , warnings = Warns
                          }
                        }
                     || C <- Clients
                    ],
                    continue
            end
    after 2000000000 ->
            finito(Clients)
    end.
