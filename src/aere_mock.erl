%-------------------------------------------------------------------
%% @doc Set of predefined ASTs for defining REPL's virtual contract
%% @end
%%%-------------------------------------------------------------------

-module(aere_mock).

-include("aere_macros.hrl").
-include("aere_repl.hrl").

-export([ eval_contract/2
        , letval_contract/4
        , letfun_contract/4
        , ann/0
        ]).


%% Default annotation
-spec ann() -> aeso_syntax:ann().
ann() ->
    [{origin, system}].

args(State) ->
    [{typed, ann(), {id, ann(), Arg}, ArgT} || {Arg, ArgT, _} <- State#repl_state.vars].

init() ->
    function([entrypoint], "init", [], {tuple, ann(), []}).

%% contract Name =
%%   Body
-spec contract(list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(Body) ->
    contract(contract_main, ?MOCK_CONTRACT, Body).
-spec contract(contract_main | contract_interface, string(), list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(ContractType, Name, Body) ->
    {ContractType, [payable, ann()], {con, ann(), Name}, [], [init()|Body]}.

%% $Attrs entrypoint $Name($Args) = $Body
-spec function(aeso_syntax:ann(), string(), [string()], aeso_syntax:expr()) -> aeso_syntax:decl().

function(Attrs, Name, Args, Body) when is_list(Attrs) ->
    { letfun
    , ann() ++ [A || A <- Attrs]
    , {id, ann(), Name}
    , Args
    , {id, ann(), "_"}
    , [{guarded, ann(), [], Body}]}.


%% Contract that evals Expr and does not chain state
-spec eval_contract(list(aeso_syntax:stmt()), repl_state()) -> aeso_syntax:ast().
eval_contract(Stmts, State) ->
    Body = {block, ann(), Stmts},
    [contract([function([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body)])].

letfun_contract(FName, Args, FBody, State) ->
    FunDef = {letfun, [entrypoint| ann()], FName, [{tuple, ann(), args(State)}|Args], {id, ann(), "_"}, FBody},
    [contract([FunDef, function([entrypoint, payable, stateful], ?USER_INPUT, [], FName)])].

-spec letval_contract(aeso_syntax:pattern(), [string()], aeso_syntax:expr(), repl_state()) -> aeso_syntax:ast().
letval_contract(Pattern, Vars, Expr, State) ->
    Let = {letval, ann(), Pattern, Expr},
    Ret = {tuple, ann(), [ {string, ann(), ?LETVAL_INDICATOR} ] ++ [{id, ann(), Var} || Var <- Vars]},
    Body = {block, ann(), [Let, Ret]},
    [contract([function([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body)])].
