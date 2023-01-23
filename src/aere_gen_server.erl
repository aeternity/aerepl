-module(aere_gen_server).

-behaviour(gen_server).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-export([ start/1
        , start_link/1
        , input/1
        , banner/0
        ]).


-define(HANDLE_ERRS(S, X), try X catch E -> server_error(S, E) end).


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
    {reply, {ok, aere_repl:print_state(State, What)}, State};

handle_call({disas, What}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, {ok, aere_repl:disassemble(What)}, State});

handle_call({break, File, Line}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, no_output, aere_debugger:add_breakpoint(State, File, Line)});

handle_call({delete_break, Index}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, no_output, aere_debugger:delete_breakpoint(State, Index)});

handle_call(Resume, _From, State)
  when Resume == continue;
       Resume == next;
       Resume == step;
       Resume == finish ->
    ?HANDLE_ERRS(State, {reply, no_output, aere_debugger:resume_eval(State, Resume)});

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
        _          -> server_error(State, aere_msg:chain_not_ready())
    end.


-spec server_error(ReplState, ErrMsg) -> {reply, {error, ErrMsg}, ReplState}
    when ReplState :: aere_repl_state:state(),
         ErrMsg    :: aere_theme:renderable().

server_error(State, E) ->
    {reply, {error, E}, State}.


-spec input(string()) -> {ok, string()} | {error, string()} | no_output | finish.

input(Input) ->
    Res =
        case aere_parse:parse(Input) of
            {error, _} = Err -> Err;
            Command          -> gen_server:call(?MODULE, Command)
        end,
    case Res of
        {ok, Msg}    -> {ok, render_message(Msg)};
        {error, Msg} -> {error, render_message(Msg)};
        Out          -> Out
    end.


-spec render_message(aere_theme:renderable()) -> string().

render_message(Message) ->
    {theme, Theme} = gen_server:call(?MODULE, theme),
    aere_theme:render(Theme, Message).


banner() ->
    gen_server:call(?MODULE, banner).
