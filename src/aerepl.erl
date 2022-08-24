-module(aerepl).

-export([start/2, stop/1]).

-behaviour(application).

start(_StartType, _StartArgs) ->
    aere_supervisor:start_link().

stop(_State) ->
    ok.
