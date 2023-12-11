-module(aere_gen_server).

-behaviour(gen_server).

%% GenServer callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%% GenServer meta
-export([ start/1
        , start/2
        , start_link/1
        , start_link/2
        ]).

%% GenServer API
-export([ quit/0
        , quit/1
        , skip/0
        , skip/1
        , reset/0
        , reset/1
        , bump_nonce/0
        , bump_nonce/1
        , blockchain_state/0
        , blockchain_state/1
        , theme/0
        , theme/1
        , type/1
        , type/2
        , state/1
        , state/2
        , eval/1
        , eval/2
        , update_filesystem_cache/1
        , update_filesystem_cache/2
        , load/1
        , load/2
        , reload/0
        , reload/1
        , set/2
        , set/3
        , help/0
        , help/1
        , help_command/1
        , help_command/2
        , lookup/1
        , lookup/2
        , disas/1
        , disas/2
        , break/2
        , break/3
        , delete_break/1
        , delete_break/2
        , delete_break_loc/2
        , delete_break_loc/3
        , resume_continue/0
        , resume_continue/1
        , resume_stepover/0
        , resume_stepover/1
        , resume_stepin/0
        , resume_stepin/1
        , resume_stepout/0
        , resume_stepout/1
        , location/0
        , location/1
        , print_var/1
        , print_var/2
        , print_vars/0
        , print_vars/1
        , stacktrace/0
        , stacktrace/1
        , banner/0
        , banner/1
        ]).

%% Complex calls
-export([ input/1
        , input/2
        , render/1
        , render/2
        , prompt/0
        , prompt/1
        ]).


-define(HANDLE_ERRS(S, X),
    try X catch
        error:E:St -> server_error(S, E, St);
        E          -> server_error(S, E)
    end).

-define(RENDER(S, Val, Format),
    case aere_repl_state:options(S) of
        #{return_mode := format} ->
            Format;
        #{return_mode := render, theme := Theme} ->
            aere_theme:render(Theme, Format);
        _ -> Val
    end).

-define(
   HANDLE_ERRS_RENDER(S0, Compute, Val, Format),
   ?HANDLE_ERRS(
      S0,
      begin
          Val = Compute,
          ?RENDER(S0, Val, Format)
      end)).

-define(
   HANDLE_ERRS_RENDER(S0, Compute, Val, S1, Format),
   ?HANDLE_ERRS(
      S0,
      begin
          {Val, S1} = Compute,
          {?RENDER(S1, Val, Format), S1}
      end)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GenServer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Args) ->
    start({local, ?MODULE}, Args).

start(Name, Args) ->
    gen_server:start(Name, ?MODULE, Args, []).

start_link(Args) ->
    start_link({local, ?MODULE}, Args).

start_link(Name, Args) ->
    gen_server:start_link(Name, ?MODULE, Args, []).


init(Args) ->
    Opts = proplists:get_value(options, Args, #{}),
    Opts1 = Opts#{init_args => Args},
    {ok, aere_repl_state:init_state(Opts1)}.


handle_call(quit, _From, State) ->
    {stop, normal, finish, State};

handle_call(skip, _From, State) ->
    {reply, ok, State};

handle_call(reset, _From, State) ->
    Opts = aere_repl_state:options(State),
    Args = maps:get(init_opts, Opts, []),
    {ok, NewState} = init(Args),
    {reply, ok, NewState};

handle_call(bump_nonce, _From, State) ->
    {reply, ok, aere_repl_state:bump_nonce(State)};

handle_call(blockchain_state, _From, State) ->
    {reply, aere_repl_state:blockchain_state(State), State};

handle_call(theme, _From, State) ->
    #{ theme := Theme } = aere_repl_state:options(State),
    {reply, {theme, Theme}, State};

handle_call({type, Expr}, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             aere_repl:infer_type(Expr, State),
             Type,
             aere_msg:type(Type)
            ),
    {reply, {ok, Out}, State};

handle_call({state, Val}, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, ok, aere_repl:set_state(Val, State)});

handle_call({eval, Code}, _From, State) ->
    ready_or_error(State),
    {Out, State2} =
        ?HANDLE_ERRS_RENDER(
           State,
           aere_repl:eval_code(Code, State),
           Res, State1,
           aere_msg:eval(Res, aere_repl_state:options(State1))
          ),
    {reply, {ok, Out}, State2};

handle_call({load, Modules}, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, ok, aere_repl:load_modules(Modules, State)});

handle_call(reload, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, ok, aere_repl:reload_modules(State)});

handle_call({set, Option, Args}, _From, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {reply, ok, aere_repl:set_option(Option, Args, State)});

handle_call(help, _From, State) ->
    Out = ?RENDER(State, aere_parse:commands(), aere_msg:help()),
    {reply, {ok, Out}, State};

handle_call({help, Command}, _From, State) ->
    Out = ?RENDER(State, aere_parse:resolve_command(Command), aere_msg:help(Command)),
    {reply, {ok, Out}, State};

handle_call({lookup, What}, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             aere_repl:lookup_state(State, What),
             Data,
             aere_msg:lookup(What, Data)
            ),
    {reply, {ok, Out}, State};

handle_call({disas, What}, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             aere_repl:disassemble(What, State),
             Fate,
             aere_msg:fate(Fate)
            ),
    {reply, {ok, Out}, State};

handle_call({break, File, Line}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, ok, aere_debugger:add_breakpoint(State, File, Line)});

handle_call({delete_break, Index}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, ok, aere_debugger:delete_breakpoint(State, Index)});

handle_call({delete_break_loc, File, Line}, _From, State) ->
    ?HANDLE_ERRS(State, {reply, ok, aere_debugger:delete_breakpoint(State, File, Line)});

handle_call(Resume, _From, State)
  when Resume == continue;
       Resume == stepover;
       Resume == stepin;
       Resume == stepout ->
    {Out, State1} =
        ?HANDLE_ERRS_RENDER(
           State,
           aere_debugger:resume_eval(State, Resume),
           Res, State0,
           aere_msg:eval(Res, aere_repl_state:options(State0))
          ),
    {reply, {ok, Out}, State1};

handle_call(location, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             #{file := FileName, line := CurrentLine} =
                 aere_debugger:source_location(State),
             L,
             aere_msg:location(FileName, CurrentLine, State)
            ),
    {reply, {ok, Out}, State};

handle_call({print_var, Var}, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             aere_debugger:lookup_variable(State, Var),
             VarR,
             aere_msg:output(VarR)
            ),
    {reply, {ok, Out}, State};

handle_call(print_vars, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             aere_debugger:get_variables(State),
             Vars,
             aere_msg:debug_vars(Vars)
            ),
    {reply, {ok, Out}, State};

handle_call(stacktrace, _From, State) ->
    Out = ?HANDLE_ERRS_RENDER(
             State,
             aere_debugger:stacktrace(State),
             Stacktrace,
             aere_msg:stacktrace(Stacktrace)
            ),
    {reply, {ok, Out}, State};

handle_call(banner, _From, State) ->
    #{ theme := Theme } = aere_repl_state:options(State),
    {reply, aere_theme:render(Theme, aere_msg:banner()), State};

handle_call(_, _, State) ->
    {reply, ok, State}.


handle_cast({update_filesystem_cache, Fs}, State) ->
    ready_or_error(State),
    ?HANDLE_ERRS(State, {noreply, aere_repl:update_filesystem_cache(Fs, State)});

handle_cast(_, State) ->
    {reply, ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quit() ->
    quit(?MODULE).
quit(ServerName) ->
    gen_server:call(ServerName, quit).

skip() ->
    skip(?MODULE).
skip(ServerName) ->
    gen_server:call(ServerName, skip).

reset() ->
    reset(?MODULE).
reset(ServerName) ->
    gen_server:call(ServerName, reset).

bump_nonce() ->
    bump_nonce(?MODULE).
bump_nonce(ServerName) ->
    gen_server:call(ServerName, bump_nonce).

blockchain_state() ->
    blockchain_state(?MODULE).
blockchain_state(ServerName) ->
    gen_server:call(ServerName, blockchain_state).

theme() ->
    theme(?MODULE).
theme(ServerName) ->
    gen_server:call(ServerName, theme).

type(Expr) ->
    type(?MODULE, Expr).
type(ServerName, Expr) ->
    gen_server:call(ServerName, {type, Expr}).

state(Val) ->
    state(?MODULE, Val).
state(ServerName, Val) ->
    gen_server:call(ServerName, {state, Val}).

eval(Code) ->
    eval(?MODULE, Code).
eval(ServerName, Code) ->
    gen_server:call(ServerName, {eval, Code}).

load(Modules) ->
    load(?MODULE, Modules).
load(ServerName, Modules) ->
    gen_server:call(ServerName, {load, Modules}).

reload() ->
    reload(?MODULE).
reload(ServerName) ->
    gen_server:call(ServerName, reload).

set(Option, Args) ->
    set(?MODULE, Option, Args).
set(ServerName, Option, Args) ->
    gen_server:call(ServerName, {set, Option, Args}).

help() ->
    help(?MODULE).
help(ServerName) ->
    gen_server:call(ServerName, help).

help_command(Command) ->
    help_command(?MODULE, Command).
help_command(ServerName, Command) ->
    gen_server:call(ServerName, {help, Command}).

lookup(What) ->
    lookup(?MODULE, What).
lookup(ServerName, What) ->
    gen_server:call(ServerName, {print, What}).

disas(What) ->
    disas(?MODULE, What).
disas(ServerName, What) ->
    gen_server:call(ServerName, {disas, What}).

break(File, Line) ->
    break(?MODULE, File, Line).
break(ServerName, File, Line) ->
    gen_server:call(ServerName, {break, File, Line}).

delete_break(Index) ->
    delete_break(?MODULE, Index).
delete_break(ServerName, Index) ->
    gen_server:call(ServerName, {delete_break, Index}).

delete_break_loc(File, Line) ->
    delete_break_loc(?MODULE, File, Line).
delete_break_loc(ServerName, File, Line) ->
    gen_server:call(ServerName, {delete_break_loc, File, Line}).

resume_continue() ->
    resume_continue(?MODULE).
resume_continue(ServerName) ->
    gen_server:call(ServerName, continue).

resume_stepover() ->
    resume_stepover(?MODULE).
resume_stepover(ServerName) ->
    gen_server:call(ServerName, stepover).

resume_stepin() ->
    resume_stepin(?MODULE).
resume_stepin(ServerName) ->
    gen_server:call(ServerName, stepin).

resume_stepout() ->
    resume_stepout(?MODULE).
resume_stepout(ServerName) ->
    gen_server:call(ServerName, stepout).

location() ->
    location(?MODULE).
location(ServerName) ->
    gen_server:call(ServerName, location).

print_var(Var) ->
    print_var(?MODULE, Var).
print_var(ServerName, Var) ->
    gen_server:call(ServerName, {print_var, Var}).

print_vars() ->
    print_var(?MODULE).
print_vars(ServerName) ->
    gen_server:call(ServerName, print_vars).

stacktrace() ->
    stacktrace(?MODULE).
stacktrace(ServerName) ->
    gen_server:call(ServerName, stacktrace).

banner() ->
    banner(?MODULE).
banner(ServerName) ->
    gen_server:call(ServerName, banner).


update_filesystem_cache(Fs) ->
    update_filesystem_cache(?MODULE, Fs).
update_filesystem_cache(ServerName, Fs) ->
    gen_server:cast(ServerName, {update_filesystem_cache, Fs}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Complex calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec render(aere_theme:renderable()) -> string().
render(Message) ->
    render(?MODULE, Message).
render(ServerName, Message) ->
    {theme, Theme} = gen_server:call(ServerName, theme),
    aere_theme:render(Theme, Message).

-spec input(string()) -> {ok, Message} | {error, Message} | no_output | finish
    when Message :: aere_theme:renderable().
input(Input) ->
    input(?MODULE, Input).
input(ServerName, Input) ->
    case aere_parse:parse(Input) of
        {error, Err} ->
            {error, render(ServerName, Err)};
        Command ->
            bump_nonce(ServerName),
            gen_server:call(ServerName, Command)
    end.

prompt() ->
    prompt(?MODULE).
prompt(ServerName) ->
    {MetaState, _} = gen_server:call(ServerName, blockchain_state),
    prompt_str(MetaState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    Msg = ?RENDER(State, Err, Err), % TODO: structured errors
    throw({reply, {error, Msg}, State});

server_error(State, {revert, Err}) ->
    Msg = ?RENDER(State, Err, aere_msg:error(Err)),
    throw({reply, {error, Msg}, State});

server_error(State, {aefa_fate, FateErr, _}) ->
    Msg = ?RENDER(
             State,
             FateErr,
             aere_msg:error(io_lib:format("FATE error: ~s", [FateErr]))
            ),
    throw({reply, {error, Msg}, State});

server_error(State, Err) ->
    Msg = ?RENDER(
             State,
             Err,
             aere_msg:error(io_lib:format("Unknown error: ~p", [Err]))
            ),
    throw({reply, {error, Msg}, State}).


-spec server_error(ReplState, Error, erlang:stacktrace()) -> {reply, {error, Msg}, ReplState}
    when ReplState :: aere_repl_state:state(),
         Error     :: term(),
         Msg       :: aere_theme:renderable().

server_error(State, Err, Stacktrace) ->
    Msg = ?RENDER(
             State,
             {Err, Stacktrace},
             aere_msg:internal(Err, Stacktrace)
            ),
    throw({reply, {error, Msg}, State}).


-spec prompt_str(BlockchainState) -> string()
    when BlockchainState :: ready | breakpoint | abort.

prompt_str(ready)      -> "AESO";
prompt_str(breakpoint) -> "AESO(DBG)";
prompt_str(abort)      -> "AESO(ABORT)".
