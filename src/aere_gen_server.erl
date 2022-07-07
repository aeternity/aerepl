-module(aere_gen_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([ init/1, handle_call/3, handle_cast/2
        %% , handle_info/2, terminate/2
        , code_change/3
        , input/1, banner/0
        , reset/0
        ]).

-include("aere_repl.hrl").

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

%%% --- GEN SERVER ---

start_link() ->
    load_deps(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, new_state()}.

handle_call({input, Input}, _From, State) ->
    {Output, State1} = process_input(Input, State),
    {reply, Output, State1};
handle_call(banner, _From, State) ->
    {reply, aere_repl:banner(State), State}.

handle_cast(reset, _) ->
    {noreply, new_state()};
handle_cast(upgrade, State) ->
    code:purge(aere_repl),
    compile:file(aere_repl, {i, "src"}),
    code:load_file(aere_repl),
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

input(Input) ->
    gen_server:call(?MODULE, {input, Input}).

banner() ->
    gen_server:call(?MODULE, banner).

reset() ->
    gen_server:cast(?MODULE, reset).


%%% --- LOGIC ---

new_state() ->
    aere_repl:init_state().

process_input(Input, State) ->
    #repl_response{
       output = Output,
       status = Status
      } = aere_repl:process_input(State, Input),
    {RetStatus, NewState} = case Status of
                 {Status1, State1 = #repl_state{}} -> {Status1, State1};
                 Status1 -> {Status1, State}
             end,
    {{RetStatus, Output}, NewState}.
