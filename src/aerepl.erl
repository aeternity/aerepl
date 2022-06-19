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
    Inp = aere_parse:get_input(fun io:get_line/1),
    {Status, Out} = aere_gen_server:input(Inp),
    io:format("~s\n", [Out]),
    case Status of
        finish -> ok;
        _ -> loop()
    end.
