%-------------------------------------------------------------------
%% @doc Set of predefined ASTs for defining REPL's virtual contract
%% @end
%%%-------------------------------------------------------------------

-module(aere_mock).

-include("aere_macros.hrl").
-include("aere_repl.hrl").

-export([ eval_contract/2
        , letval_contract/4
        , ann/0
        ]).


%% Default annotation
-spec ann() -> aeso_syntax:ann().
ann() ->
    [{origin, system}].

args(State) ->
    [{typed, ann(), {id, ann(), Arg}, ArgT} || {Arg, ArgT, _} <- State#repl_state.vars].

%% contract Name =
%%   Body
-spec contract(list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(Body) ->
    contract(contract_main, ?MOCK_CONTRACT, Body).
-spec contract(contract_main | contract_interface, string(), list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(ContractType, Name, Body) ->
    {ContractType, [payable, ann()], {con, ann(), Name}, [], Body}.

%% $Attrs entrypoint $Name($Args) = $Body
-spec entrypoint(aeso_syntax:ann(), string(), [string()], aeso_syntax:expr()) -> aeso_syntax:decl().

entrypoint(Attrs, Name, Args, Body) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , Args
    , {id, ann(), "_"}
    , [{guarded, ann(), [], Body}]}.


%% Contract that evals Expr and does not chain state
-spec eval_contract(list(aeso_syntax:stmt()), repl_state()) -> aeso_syntax:ast().
eval_contract(Stmts, State) ->
    Body = {block, ann(), Stmts},
    [contract([entrypoint([payable, stateful], ?USER_INPUT, args(State), Body)])].

-spec letval_contract(aeso_syntax:pattern(), [string()], aeso_syntax:expr(), repl_state()) -> aeso_syntax:ast().
letval_contract(Pattern, Vars, Expr, State) ->
    Let = {letval, ann(), Pattern, Expr},
    Ret = {tuple, ann(), [{id, ann(), Var} || Var <- Vars]},
    Body = {block, ann(), [Let, Ret]},
    [contract([entrypoint([payable, stateful], ?USER_INPUT, args(State), Body)])].
