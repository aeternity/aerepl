-module(aere_gen_server).
-behaviour(gen_server).
-export([start/1, start_link/1]).
-export([ init/1, handle_call/3, handle_cast/2
        %% , handle_info/2, terminate/2
        , code_change/3
        , input/1, banner/0
        , reset/0
        ]).

-include("aere_repl.hrl").

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
    end,
    ok = application:load(aechannel),
    ok = application:load(aecontract),
    ok = application:load(aecore),
    ok = application:load(aefate),
    ok = application:load(aens),
    ok = application:load(aeoracle),
    ok = application:load(aeprimop),
    ok = application:load(aetx),
    ok = application:load(aeutils),
    ok = application:load(setup).


%%% --- GEN SERVER ---

start(Args) ->
    load_deps(),
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
    load_deps(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_) ->
    {ok, new_state()}.

handle_call({input, Input}, _From, State) ->
    {Output, State1} = process_input(Input, State),
    {reply, Output, State1};
handle_call(banner, _From, State = #repl_state{options = #{theme := Theme}}) ->
    {reply, aere_theme:render(Theme, aere_msg:banner()), State}.

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

process_input(Input, State = #repl_state{options = #{theme := Theme}}) ->
    #repl_response{
       output = Output,
       status = Status
      } = aere_repl:process_input(State, Input),
    {RetStatus, NewState} =
        case Status of
            {Status1, State1 = #repl_state{}} -> {Status1, State1};
            Status1                           -> {Status1, State}
        end,
    {{RetStatus, aere_theme:render(Theme, Output)}, NewState}.
