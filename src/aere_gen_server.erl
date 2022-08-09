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

%%% --- GEN SERVER ---

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
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
