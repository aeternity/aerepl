-module(aere_cli).

-export([run/0, load_deps/0]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").


load_deps() ->
    code:add_pathz("node/_build/dev1/lib/aechannel/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aecontract/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aecore/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aefate/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aens/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aeoracle/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aeprimop/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aetx/ebin/"),
    code:add_pathz("node/_build/dev1/lib/aeutils/ebin/"),
    code:add_pathz("node/_build/dev1/lib/setup/ebin/"),
    application:load(aechannel),
    application:load(aecontract),
    application:load(aecore),
    application:load(aefate),
    application:load(aens),
    application:load(aeoracle),
    application:load(aeprimop),
    application:load(aetx),
    application:load(aeutils),
    application:load(setup).


run() ->
    aere_gen_server:start_link(),
    Banner = gen_server:call(aere_gen_server, banner),
    io:format(Banner),
    loop().


loop() ->
    Inp = aere_parse:get_input(fun io:get_line/1),
    {Status, Out} = aere_gen_server:input(Inp),
    io:format("~s\n", [Out]),
    case Status of
        quit -> ok;
        _ -> loop()
    end.
