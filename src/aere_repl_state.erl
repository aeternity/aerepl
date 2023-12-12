%%%-------------------------------------------------------------------
%% @doc Getters and setters of the REPL state for external use.
%% @end
%%%-------------------------------------------------------------------
-module(aere_repl_state).

-include("aere_macros.hrl").

-type print_format() :: sophia | fate | json.
-type return_mode() :: value | format | render.
-type repl_options() ::
        #{ theme       := aere_theme:theme()
         , display_gas  := boolean()
         , call_gas     := pos_integer()
         , call_value   := non_neg_integer()
         , print_format := print_format()
         , return_mode  := return_mode()
         , print_unit   := boolean()
         , print_type   := boolean()
         , loc_backwards := non_neg_integer()
         , loc_forwards  := non_neg_integer()
         , locked_opts  => [atom()]
         , init_args    => [any()]
         }.
-type breakpoints() :: [{string(), integer()}].
-type function_symbols() :: #{binary() => binary()}.
-type var() :: {string(), aeso_syntax:type(), term()}.
-type contract_state() :: {aeso_syntax:type(), aeb_fate_data:fate_type()}.
-type type_def() :: {string(), string(), [aeso_syntax:tvar()], aeso_syntax:typedef()}.
-type type_scope() :: {string(), {string(), non_neg_integer()}}.
-type chain_state() :: {ready, aefa_chain_api:state()}
                     | {breakpoint, aefa_engine_state:state()}
                     | {abort, aefa_engine_state:state()}.
-type filesystem() :: local | {cached, #{string() => binary()}}.

-record(rs,
        { blockchain_state       :: chain_state()
        , repl_account           :: binary()
        , options                :: repl_options()
        , contract_state         :: contract_state()
        , vars           = []    :: [var()]
        , funs           = #{}   :: #{binary() => term()}
        , typedefs       = []    :: [type_def()]
        , type_scope     = []    :: [type_scope()]
        , filesystem     = local :: filesystem() % Whether to load files from disc or pre-defined map
        , loaded_files   = #{}   :: #{string() => binary()} % Loaded files ready to be included
        , included_files = []    :: [string()] % Files included in the context
        , included_code  = []    :: aeso_syntax:ast() % Cached AST of the included files
        , query_nonce    = 0     :: non_neg_integer()
        , breakpoints    = []    :: breakpoints()
        , function_symbols = #{} :: #{binary() => binary()}
        , type_env = none        :: none | term()
        }).

-opaque state() :: #rs{}.

-export_type(
   [ state/0, type_scope/0, type_def/0, repl_options/0
   , function_symbols/0, breakpoints/0]).

-export([ init_state/0, init_state/1
        , init_options/0
        ]).

%% Getters
-export([ blockchain_state/1
        , repl_account/1
        , options/1
        , contract_state/1
        , vars/1
        , funs/1
        , typedefs/1
        , type_scope/1
        , filesystem/1
        , loaded_files/1
        , included_files/1
        , included_code/1
        , query_nonce/1
        , breakpoints/1
        , function_symbols/1
        , type_env/1
        ]).

%% Setters
-export([ set_blockchain_state/2
        , set_repl_account/2
        , set_options/2
        , set_contract_state/2
        , set_vars/2
        , set_funs/2
        , set_typedefs/2
        , set_type_scope/2
        , set_filesystem/2
        , set_loaded_files/2
        , set_included_files/2
        , set_included_code/2
        , set_query_nonce/2
        , set_breakpoints/2
        , set_function_symbols/2
        , set_type_env/2
        ]).

-export([ chain_api/1
        , bump_nonce/1
        , update_cached_fs/2
        ]).

-spec init_options() -> repl_options().
init_options() ->
    #{ theme => aere_theme:default_theme()
     , display_gas   => false
     , call_gas      => 100000000000000000
     , call_value    => 0
     , print_format  => sophia
     , return_mode   => value
     , print_unit    => false
     , print_type    => false
     , loc_backwards => 5
     , loc_forwards  => 5
     , locked_opts   => []
    }.

-spec init_state() -> state().
init_state() ->
    init_state(init_options()).

-spec init_state(repl_options()) -> state().
init_state(Opts) ->
    Trees0 = aec_trees:new(),
    {_, Trees} = aere_chain:new_account(100000000000000000000000000000, Trees0),
    PK = <<0:256>>,
    ChainState = aefa_chain_api:new(
                   #{ gas_price => 1,
                      fee       => 0,
                      trees     => Trees,
                      origin    => PK,
                      tx_env    => aere_chain:default_tx_env(1)
                   }
                  ),
    S0 = #rs{
       blockchain_state = {ready, ChainState},
       repl_account     = PK,
       options          = maps:merge(init_options(), Opts),
       filesystem       = maps:get(filesystem, Opts, local),
       contract_state   = ?DEFAULT_CONTRACT_STATE
      },
    S0.

-spec blockchain_state(state()) -> chain_state().
blockchain_state(#rs{blockchain_state = BlockchainState}) ->
    BlockchainState.

-spec set_blockchain_state(chain_state(), state()) -> state().
set_blockchain_state(X, S) ->
    S#rs{blockchain_state = X}.

-spec repl_account(state()) -> binary().
repl_account(#rs{repl_account = ReplAccount}) ->
    ReplAccount.

-spec set_repl_account(binary(), state()) -> state().
set_repl_account(X, S) ->
    S#rs{repl_account = X}.

-spec options(state()) -> repl_options().
options(#rs{options = Options}) ->
    Options.

-spec set_options(repl_options(), state()) -> state().
set_options(X, S) ->
    S#rs{options = X}.

-spec contract_state(state()) -> contract_state().
contract_state(#rs{contract_state = ContractState}) ->
    ContractState.

-spec set_contract_state(contract_state(), state()) -> state().
set_contract_state(X, S) ->
    S#rs{contract_state = X}.

-spec vars(state()) -> [var()].
vars(#rs{vars = Vars}) ->
    Vars.

-spec set_vars([var()], state()) -> state().
set_vars(X, S) ->
    S#rs{vars = X}.

-spec funs(state()) -> #{binary() => term()}.
funs(#rs{funs = Funs}) ->
    Funs.

-spec set_funs(#{binary() => term()}, state()) -> state().
set_funs(X, S) ->
    S#rs{funs = X}.

-spec typedefs(state()) -> [type_def()].
typedefs(#rs{typedefs = Typedefs}) ->
    Typedefs.

-spec set_typedefs([type_def()], state()) -> state().
set_typedefs(X, S) ->
    S#rs{typedefs = X}.

-spec type_scope(state()) -> [type_scope()].
type_scope(#rs{type_scope = TypeScope}) ->
    TypeScope.

-spec set_type_scope([type_scope()], state()) -> state().
set_type_scope(X, S) ->
    S#rs{type_scope = X}.

-spec type_env(state()) -> term().
type_env(#rs{type_env = TypeEnv}) ->
    TypeEnv.

-spec set_type_env(term(), state()) -> state().
set_type_env(X, S) ->
    S#rs{type_env = X}.

-spec filesystem(state()) -> filesystem().
filesystem(#rs{filesystem = Fs}) ->
    Fs.

-spec set_filesystem(filesystem(), state()) -> state().
set_filesystem(Fs, S) ->
    S#rs{filesystem = Fs}.

-spec loaded_files(state()) -> #{string() => binary()}.
loaded_files(#rs{loaded_files = LoadedFiles}) ->
    LoadedFiles.

-spec set_loaded_files(#{string() => binary()}, state()) -> state().
set_loaded_files(X, S) ->
    S#rs{loaded_files = X}.

-spec included_files(state()) -> [string()].
included_files(#rs{included_files = IncludedFiles}) ->
    IncludedFiles.

-spec set_included_files([string()], state()) -> state().
set_included_files(X, S) ->
    S#rs{included_files = X}.

-spec included_code(state()) -> aeso_syntax:ast().
included_code(#rs{included_code = IncludedCode}) ->
    IncludedCode.

-spec set_included_code(aeso_syntax:ast(), state()) -> state().
set_included_code(X, S) ->
    S#rs{included_code = X}.

-spec query_nonce(state()) -> non_neg_integer().
query_nonce(#rs{query_nonce = QueryNonce}) ->
    QueryNonce.

-spec set_query_nonce(non_neg_integer(), state()) -> state().
set_query_nonce(X, S) ->
    S#rs{query_nonce = X}.

-spec breakpoints(state()) -> breakpoints().
breakpoints(#rs{breakpoints = Breakpoints}) ->
    Breakpoints.

-spec set_breakpoints(breakpoints(), state()) -> state().
set_breakpoints(Breakpoints, S) ->
    S#rs{breakpoints = Breakpoints}.

-spec function_symbols(state()) -> function_symbols().
function_symbols(#rs{function_symbols = Symbols}) ->
    Symbols.

-spec set_function_symbols(function_symbols(), state()) -> state().
set_function_symbols(Symbols, S) ->
    S#rs{function_symbols = Symbols}.

%% Advanced getters

-spec chain_api(state()) -> aefa_chain_api:state().
chain_api(#rs{blockchain_state = {ready, Api}}) ->
    Api;
chain_api(#rs{blockchain_state = {breakpoint, ES}}) ->
    aefa_engine_state:chain_api(ES);
chain_api(#rs{blockchain_state = {abort, ES}}) ->
    aefa_engine_state:chain_api(ES).

-spec bump_nonce(state()) -> state().
bump_nonce(S = #rs{query_nonce = N}) ->
    S#rs{query_nonce = N + 1}.

-spec update_cached_fs(#{string() => binary()}, state()) -> error | {ok, state()}.
update_cached_fs(_, #rs{filesystem = local}) ->
    error;
update_cached_fs(Fs, S) when is_map(Fs) ->
    {ok,  S#rs{filesystem = {cached, Fs}}}.
