%%%-------------------------------------------------------------------
%% @doc Getters and setters of the REPL state for external use.
%% @end
%%%-------------------------------------------------------------------
-module(aere_repl_state).

%% We want to keep the record definition available for internal use.
-include("aere_repl.hrl").

-opaque state() :: repl_state().

-export_type([state/0]).

-export([ blockchain_state/1, blockchain_state/2
        , repl_account/1, repl_account/2
        , options/1, options/2
        , contract_state/1, contract_state/2
        , vars/1, vars/2
        , funs/1, funs/2
        , typedefs/1, typedefs/2
        , type_scope/1, type_scope/2
        , loaded_files/1, loaded_files/2
        , included_files/1, included_files/2
        , included_code/1, included_code/2
        , query_nonce/1, query_nonce/2
        ]).

-spec blockchain_state(state()) -> chain_state().
blockchain_state(S) ->
    S#repl_state.blockchain_state.

-spec blockchain_state(chain_state(), state()) -> state().
blockchain_state(X, S) ->
    S#repl_state{blockchain_state = X}.

-spec repl_account(state()) -> binary().
repl_account(S) ->
    S#repl_state.repl_account.

-spec repl_account(binary(), state()) -> state().
repl_account(X, S) ->
    S#repl_state{repl_account = X}.

-spec options(state()) -> repl_options().
options(S) ->
    S#repl_state.options.

-spec options(repl_options(), state()) -> state().
options(X, S) ->
    S#repl_state{options = X}.

-spec contract_state(state()) -> contract_state().
contract_state(S) ->
    S#repl_state.contract_state.

-spec contract_state(contract_state(), state()) -> state().
contract_state(X, S) ->
    S#repl_state{contract_state = X}.

-spec vars(state()) -> [var()].
vars(S) ->
    S#repl_state.vars.

-spec vars([var()], state()) -> state().
vars(X, S) ->
    S#repl_state{vars = X}.

-spec funs(state()) -> #{binary() => term()}.
funs(S) ->
    S#repl_state.funs.

-spec funs(#{binary() => term()}, state()) -> state().
funs(X, S) ->
    S#repl_state{funs = X}.

-spec typedefs(state()) -> [type_def()].
typedefs(S) ->
    S#repl_state.typedefs.

-spec typedefs([type_def()], state()) -> state().
typedefs(X, S) ->
    S#repl_state{typedefs = X}.

-spec type_scope(state()) -> [type_scope()].
type_scope(S) ->
    S#repl_state.type_scope.

-spec type_scope([type_scope()], state()) -> state().
type_scope(X, S) ->
    S#repl_state{type_scope = X}.

-spec loaded_files(state()) -> #{string() => binary()}.
loaded_files(S) ->
    S#repl_state.loaded_files.

-spec loaded_files(#{string() => binary()}, state()) -> state().
loaded_files(X, S) ->
    S#repl_state{loaded_files = X}.

-spec included_files(state()) -> [string()].
included_files(S) ->
    S#repl_state.included_files.

-spec included_files([string()], state()) -> state().
included_files(X, S) ->
    S#repl_state{included_files = X}.

-spec included_code(state()) -> aeso_syntax:ast().
included_code(S) ->
    S#repl_state.included_code.

-spec included_code(aeso_syntax:ast(), state()) -> state().
included_code(X, S) ->
    S#repl_state{included_code = X}.

-spec query_nonce(state()) -> non_neg_integer().
query_nonce(S) ->
    S#repl_state.query_nonce.

-spec query_nonce(non_neg_integer(), state()) -> state().
query_nonce(X, S) ->
    S#repl_state{query_nonce = X}.
