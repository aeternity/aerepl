-module(aere_supervisor).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    Flags = #{strategy => one_for_one,
              intensity => 1,
              period => 3
            },
    ChildSpecs =
        [ #{id => aerepl,
            start => {aere_gen_server, start_link, [[]]},
            restart => permanent,
            significant => false,
            shutdown => brutal_kill,
            type => worker,
            modules => [aere_gen_server]
          }
        ],
    {ok, {Flags, ChildSpecs}}.
