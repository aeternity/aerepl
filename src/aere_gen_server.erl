-module(aere_gen_server).
-behaviour(gen_server).
-export([start/1, start_link/1]).
-export([ init/1, handle_call/3, handle_cast/2
        %% , handle_info/2, terminate/2
        , input/1, input/2, banner/0
        , reset/0
        ]).

%%% --- GEN SERVER ---

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    {ok, new_state(Args)}.

handle_call({input, Input}, _From, State) ->
    {Output, State1} = process_input(Input, State),
    {reply, Output, State1};
handle_call(banner, _From, State) ->
    #{ theme := Theme } = aere_repl_state:options(State),
    {reply, aere_theme:render(Theme, aere_msg:banner()), State};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(reset, _) ->
    {noreply, new_state()};
handle_cast(_, State) ->
    {noreply, State}.

input(Input) ->
    gen_server:call(?MODULE, {input, Input}).

input(Input, State) ->
    gen_server:call(?MODULE, {input, Input, State}).

banner() ->
    gen_server:call(?MODULE, banner).

reset() ->
    gen_server:cast(?MODULE, reset).


%%% --- LOGIC ---

new_state() ->
    new_state([]).
new_state(Args) ->
    Opts = proplists:get_value(options, Args, #{}),
    aere_repl_state:init_state(Opts).

process_input(Input, State) ->
    #{ theme := Theme } = aere_repl_state:options(State),
    Response = aere_repl:process_input(State, Input),
    {RetStatus, NewState} =
        case aere_repl_response:status(Response) of
            {ok, State1} -> {ok, State1};
            Status       -> {Status, State}
        end,
    {{RetStatus, aere_theme:render(Theme, aere_repl_response:output(Response))}, NewState}.
