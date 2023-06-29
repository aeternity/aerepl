-module(aere_gen_server).

-behaviour(gen_server).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-export([ start/1
        , start_link/1
        , input/1
        , render/1
        , banner/0
        , restart/0
        ]).


-define(HANDLE_ERRS(S, X),
    try X catch
        error:E:St -> server_error(S, E, St);
        E          -> server_error(S, E)
    end).


%%% --- GEN SERVER ---

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).


start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


init(Args) ->
    Opts = proplists:get_value(options, Args, #{}),
    {ok, aere_repl_state:init_state(Opts)}.


handle_call(quit, _From, State) ->
    {reply, finish, State};

handle_call(skip, _From, State) ->
    {reply, no_output, State};

handle_call(reset, _From, _State) ->
    {reply, no_output, aere_repl_state:init_state()};

handle_call(bump_nonce, _From, State) ->
    {reply, ok, aere_repl_state:bump_nonce(State)};

handle_call(blockchain_state, _From, State) ->
    {reply, {ok, element(1, aere_repl_state:blockchain_state(State))}, State};

handle_call(theme, _From, State) ->
    #{ theme := Theme } = aere_repl_state:options(State),
    {reply, {theme, Theme}, State};

handle_call({type, Expr}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_repl:infer_type(Expr, State)}, State});

handle_call({state, Val}, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, no_output, aere_repl:set_state(Val, State)});

handle_call({eval, Code}, _From, State) ->
    ready_or_error(State),
    case ?HANDLE_ERRS(State, aere_repl:eval_code(Code, State)) of
        {Msg, NewState} -> {reply, {ok, Msg}, NewState};
        NewState        -> {reply, no_output, NewState}
    end;

handle_call({load, Modules}, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, no_output, aere_repl:load_modules(Modules, State)});

handle_call(reload, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, no_output, aere_repl:reload_modules(State)});

handle_call({set, Option, Args}, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, no_output, aere_repl:set_option(Option, Args, State)});

handle_call(help, _From, State) ->
    {reply, {ok, aere_msg:help()}, State};

handle_call({help, Command}, _From, State) ->
    {reply, {ok, aere_msg:help(Command)}, State};

handle_call({print, What}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_repl:print_state(State, What)}, State});

handle_call({disas, What}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_repl:disassemble(What, State)}, State});

handle_call({break, File, Line}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, no_output, aere_debugger:add_breakpoint(State, File, Line)});

handle_call({delete_break, Index}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, no_output, aere_debugger:delete_breakpoint(State, Index)});

handle_call({delete_break_loc, File, Line}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, no_output, aere_debugger:delete_breakpoint(State, File, Line)});

handle_call(Resume, _From, State)
  when Resume == continue;
       Resume == stepover;
       Resume == stepin;
       Resume == stepout ->
    {Out, NewState} = ?HANDLE_ERRS(State, aere_debugger:resume_eval(State, Resume)),
    {reply, {ok, Out}, NewState};

handle_call(location, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_debugger:source_location(State)}, State} );

handle_call({print_var, Var}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_debugger:lookup_variable(State, Var)}, State});

handle_call(stacktrace, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_debugger:stacktrace(State)}, State});

handle_call(banner, _From, State) ->
    #{ theme := Theme } = aere_repl_state:options(State),
    {reply, aere_theme:render(Theme, aere_msg:banner()), State};

handle_call(_, _, State) ->
    {noreply, State}.


handle_cast(_, State) ->
    {noreply, State}.


%%% --- LOGIC ---

-spec ready_or_error(ReplState) -> ok | {reply, {error, ErrMsg}, ReplState}
    when ReplState :: aere_repl_state:state(),
         ErrMsg    :: aere_theme:renderable().

ready_or_error(State) ->
    case aere_repl_state:blockchain_state(State) of
        {ready, _} -> ok;
        _          -> server_error(State, {repl_error, aere_msg:chain_not_ready()})
    end.


-spec server_error(ReplState, Error) -> {reply, {error, Msg}, ReplState}
    when ReplState :: aere_repl_state:state(),
         Error     :: term(),
         Msg       :: aere_theme:renderable().

server_error(State, {repl_error, Err}) ->
    throw({reply, {error, Err}, State});

server_error(State, {revert, Err}) ->
    throw({reply, {error, aere_msg:error(Err)}, State});

server_error(State, {aefa_fate, FateErr, _}) ->
    throw({reply, {error, aere_msg:error(io_lib:format("FATE error: ~s", [FateErr]))}, State});

server_error(State, Err) ->
    throw({reply, {error, aere_msg:error(io_lib:format("Unknown error: ~p", [Err]))}, State}).


-spec server_error(ReplState, Error, erlang:stacktrace()) -> {reply, {error, Msg}, ReplState}
    when ReplState :: aere_repl_state:state(),
         Error     :: term(),
         Msg       :: aere_theme:renderable().

server_error(State, Err, Stacktrace) ->
    throw({reply, {error, aere_msg:internal(Err, Stacktrace)}, State}).


-spec prompt_str(BlockchainState) -> string()
    when BlockchainState :: ready | breakpoint | abort.

prompt_str(ready)      -> "AESO";
prompt_str(breakpoint) -> "AESO(DBG)";
prompt_str(abort)      -> "AESO(ABORT)".


-spec input(string()) -> {ok, Message} | {error, Message} | no_output | finish
    when Message :: aere_theme:renderable().

input(Input) ->
    {ok, BCState} = gen_server:call(?MODULE, blockchain_state),
    case aere_parse:parse(Input(prompt_str(BCState))) of
        Err = {error, _} ->
            Err;
        Command ->
            gen_server:call(?MODULE, bump_nonce),
            gen_server:call(?MODULE, Command)
    end.


-spec render(aere_theme:renderable()) -> string().

render(Message) ->
    {theme, Theme} = gen_server:call(?MODULE, theme),
    aere_theme:render(Theme, Message).


-spec restart() -> ok.

restart() ->
    gen_server:call(?MODULE, reset).


banner() ->
    gen_server:call(?MODULE, banner).
