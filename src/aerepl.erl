-module(aerepl).

-export([main/1, start/0]).

main(_Args) ->
    start().

start() ->
    erlang:system_flag(backtrace_depth, 100),
    {ok, _} = aere_gen_server:start_link(),
    Banner = gen_server:call(aere_gen_server, banner),
    io:format(Banner ++ "\n\n"),
    loop().

loop() ->
    Inp = aere_parse:get_input(),
    {Status, Out} = aere_gen_server:input(Inp),
    case Status of
        finish -> ok;
        skip -> loop();
        _ ->
            io:format("~s\n", [Out]),
            loop()
    end.
