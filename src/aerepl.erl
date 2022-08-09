-module(aerepl).

-export([start/2, stop/1]).

-behaviour(application).

load_deps() ->
    case
        code:add_pathz("node/_build/dev1/lib/aechannel/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aecontract/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aecore/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aefate/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aens/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aeoracle/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aeprimop/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aetx/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/aeutils/ebin/") andalso
        code:add_pathz("node/_build/dev1/lib/setup/ebin/") of
        true -> ok;
        Err -> throw(Err)
    end.

start(_StartType, _StartArgs) ->
    load_deps(),
    aere_supervisor:start_link().

stop(_State) ->
    ok.
