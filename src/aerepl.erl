-module(aerepl).

-export([ main/1, start/0, loop/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

main(_Args) ->
    start().

start() ->
    erlang:system_flag(backtrace_depth, 100),
    loop([]).

-spec init_message() -> {response, pid(), repl_response()}.
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
            continue
    end.
