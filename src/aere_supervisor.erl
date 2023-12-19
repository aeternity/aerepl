-module(aere_supervisor).

-behaviour(supervisor).

-export([start_link/0, start_link/1, terminate/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(ReplArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ReplArgs).

init(ReplArgs) ->
    Flags = #{strategy => one_for_one,
              intensity => 5,
              period => 3
            },
    ChildSpecs =
        [ #{id => aerepl,
            start => {aere_gen_server, start_link, [ReplArgs]},
            restart => permanent,
            significant => false,
            shutdown => brutal_kill,
            type => worker,
            modules => [aere_gen_server]
          }
        ],
    {ok, {Flags, ChildSpecs}}.

terminate(Pid) ->
    supervisor:terminate_child(Pid, aerepl),
    supervisor:delete_child(Pid, aerepl),
    ok.
