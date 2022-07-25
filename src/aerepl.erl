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
    print_msg(Out),
    case Status of
        finish -> ok;
        skip -> loop();
        ok -> loop();
        error -> loop();
        internal_error -> loop()
    end.

print_msg("") -> ok;
print_msg(Msg) ->
    io:format("~s\n", [Msg]).
