-module(aerepl).

-export([ main/1, start/0, init_worker/1
        , join/1, query/3, answer/2, get/1]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

main(_Args) ->
    start().

start() ->
    erlang:system_flag(backtrace_depth, 100),
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    wait_for_connections(maps:new()).

join(R) ->
    R ! {start, self()}.
query(R, S, Msg) ->
    R ! {input, self(), S, Msg}.
answer(R, Msg) ->
    R ! {answer, self(), Msg}.
get(R) ->
    receive
        {response, R, Res = #repl_response{}} -> Res
    end.

wait_for_connections(Users) ->
    receive
        {start, Who} when is_pid(Who) ->
            Proc = spawn(aerepl, init_worker, [Who]),
            Who ! {your_repl, Proc},
            wait_for_connections(maps:put(Who, Proc, Users));
        finito ->
            finito;
        _ -> wait_for_connections(Users)
    end.

init_worker(User) ->
    User !
        {response, self(), #repl_response
         { output = aere_repl:banner()
         , warnings = []
         , status = {success, aere_repl:init_state()}
         }},
    loop(User).
loop(User) ->
    {State, Inp} = receive
              {input, User, S = #repl_state{}, I} -> {S, I}
          end,
    case aere_repl:process_string(State, Inp) of
        Resp = #repl_response{} ->
            User ! {response, self(), Resp},
            loop(User);
        Q = #repl_question{} ->
            QR = fun Retry(MyQ = #repl_question{}) ->
                         User ! {response, self(), aere_repl:question_to_response(MyQ)},
                         Ans = receive
                                   {answer, User, A} -> A
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
            loop(User)
    end.
