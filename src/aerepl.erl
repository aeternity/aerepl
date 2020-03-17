-module(aerepl).

-export([ main/1, start/0, init_worker/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

main(_Args) ->
    start().

start() ->
    erlang:system_flag(backtrace_depth, 100),
    wait_for_connections(maps:new()).

wait_for_connections(Users) ->
    receive
        {start, Who} when is_pid(Who) ->
            Proc = spawn(aerepl, init_worker, [Who]),
            Who ! {your_repl, Proc},
            wait_for_connections(maps:put(Who, Proc, Users));
        {goodbye, Dude} ->
            wait_for_connections(maps:remove(Dude, Users));
        finito ->
            finito;
        _ -> wait_for_connections(Users)
    end.

init_message() ->
    {response, self(), #repl_response
     { output = aere_repl:banner()
     , warnings = []
     , status = {success, aere_repl:init_state()}
     }}.

init_worker(User) ->
    User ! init_message(),
    loop(User, self()).

loop(User, Daddy) ->
    {State, Inp} =
        receive
            {input, User, S = #repl_state{}, I} ->
                {S, I}
        after 2000000000 ->
                Daddy ! {goodbye, User},
                throw(timeout)
        end,
    case aere_repl:process_string(State, Inp) of
        Resp = #repl_response{} ->
            User ! {response, self(), Resp},
            loop(User, Daddy);
        Q = #repl_question{} ->
            QR = fun Retry(MyQ = #repl_question{}) ->
                         User ! {response, self(), aere_repl:question_to_response(MyQ)},
                         Ans = receive
                                   {answer, User, A} -> A
                               after 2000000000 ->
                                       Daddy ! {goodbye, User},
                                       throw(timeout)
                               end,
                         case aere_repl:answer(MyQ, Ans) of
                             {retry, MyQ1} -> Retry(MyQ1);
                             {accept, X} -> X
                         end
                 end,
            {State1, Warns} = aere_repl:destroy_warnings(QR(Q)),
            User !
                { response
                , self()
                , #repl_response
                  { output = ""
                  , status = {success, State1}
                  , warnings = Warns
                  }
                },
            loop(User, Daddy);
        finito ->
            Daddy ! {goodbye, User},
            finito;
        _ -> loop(User, Daddy)
    end.
