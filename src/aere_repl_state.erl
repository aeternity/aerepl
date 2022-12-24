%%%-------------------------------------------------------------------
%% @doc Getters and setters of the REPL state for external use.
%% @end
%%%-------------------------------------------------------------------
-module(aere_repl_state).

%% We want to keep the record definition available for internal use.
-include("aere_repl.hrl").

-type state() :: repl_state().

-export_type([state/0]).

%% Getters
-export([ blockchain_state/1
        , repl_account/1
        , options/1
        , contract_state/1
        , vars/1
        , funs/1
        , typedefs/1
        , type_scope/1
        , loaded_files/1
        , included_files/1
        , included_code/1
        , query_nonce/1
        , breakpoints/1
        , callback/1
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
        , set_loaded_files/2
        , set_included_files/2
        , set_included_code/2
        , set_query_nonce/2
        , set_breakpoints/2
        , set_callback/2
        ]).

-export([ chain_api/1
        ]).

-spec blockchain_state(state()) -> chain_state().
blockchain_state(#repl_state{blockchain_state = BlockchainState}) ->
    BlockchainState.

-spec set_blockchain_state(chain_state(), state()) -> state().
set_blockchain_state(X, S) ->
    S#repl_state{blockchain_state = X}.

-spec repl_account(state()) -> binary().
repl_account(#repl_state{repl_account = ReplAccount}) ->
    ReplAccount.

-spec set_repl_account(binary(), state()) -> state().
set_repl_account(X, S) ->
    S#repl_state{repl_account = X}.

-spec options(state()) -> repl_options().
options(#repl_state{options = Options}) ->
    Options.

-spec set_options(repl_options(), state()) -> state().
set_options(X, S) ->
    S#repl_state{options = X}.

-spec contract_state(state()) -> contract_state().
contract_state(#repl_state{contract_state = ContractState}) ->
    ContractState.

-spec set_contract_state(contract_state(), state()) -> state().
set_contract_state(X, S) ->
    S#repl_state{contract_state = X}.

-spec vars(state()) -> [var()].
vars(#repl_state{vars = Vars}) ->
    Vars.

-spec set_vars([var()], state()) -> state().
set_vars(X, S) ->
    S#repl_state{vars = X}.

-spec funs(state()) -> #{binary() => term()}.
funs(#repl_state{funs = Funs}) ->
    Funs.

-spec set_funs(#{binary() => term()}, state()) -> state().
set_funs(X, S) ->
    S#repl_state{funs = X}.

-spec typedefs(state()) -> [type_def()].
typedefs(#repl_state{typedefs = Typedefs}) ->
    Typedefs.

-spec set_typedefs([type_def()], state()) -> state().
set_typedefs(X, S) ->
    S#repl_state{typedefs = X}.

-spec type_scope(state()) -> [type_scope()].
type_scope(#repl_state{type_scope = TypeScope}) ->
    TypeScope.

-spec set_type_scope([type_scope()], state()) -> state().
set_type_scope(X, S) ->
    S#repl_state{type_scope = X}.

-spec loaded_files(state()) -> #{string() => binary()}.
loaded_files(#repl_state{loaded_files = LoadedFiles}) ->
    LoadedFiles.

-spec set_loaded_files(#{string() => binary()}, state()) -> state().
set_loaded_files(X, S) ->
    S#repl_state{loaded_files = X}.

-spec included_files(state()) -> [string()].
included_files(#repl_state{included_files = IncludedFiles}) ->
    IncludedFiles.

-spec set_included_files([string()], state()) -> state().
set_included_files(X, S) ->
    S#repl_state{included_files = X}.

-spec included_code(state()) -> aeso_syntax:ast().
included_code(#repl_state{included_code = IncludedCode}) ->
    IncludedCode.

-spec set_included_code(aeso_syntax:ast(), state()) -> state().
set_included_code(X, S) ->
    S#repl_state{included_code = X}.

-spec query_nonce(state()) -> non_neg_integer().
query_nonce(#repl_state{query_nonce = QueryNonce}) ->
    QueryNonce.

-spec set_query_nonce(non_neg_integer(), state()) -> state().
set_query_nonce(X, S) ->
    S#repl_state{query_nonce = X}.

-spec breakpoints(state()) -> breakpoints().
breakpoints(#repl_state{breakpoints = Breakpoints}) ->
    Breakpoints.

-spec set_breakpoints(breakpoints(), state()) -> state().
set_breakpoints(Breakpoints, RS) ->
    RS#repl_state{breakpoints = Breakpoints}.

-spec callback(state()) -> callback().
callback(#repl_state{callback = Callback}) ->
    Callback.

-spec set_callback(callback(), state()) -> state().
set_callback(Callback, RS) ->
    RS#repl_state{callback = Callback}.

%% Advanced getters

-spec chain_api(state()) -> aefa_chain_api:state().
chain_api(#repl_state{blockchain_state = {ready, Api}}) ->
    Api;
chain_api(#repl_state{blockchain_state = {running, Api, _, _}}) ->
    Api;
chain_api(#repl_state{blockchain_state = {breakpoint, ES}}) ->
    aefa_engine_state:chain_api(ES).
