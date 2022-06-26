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
        , typedef_contract/4
        , type_unfold_contract/1
        , type_unfold_contract/2
        , ann/0
        ]).


%% Default annotation
-spec ann() -> aeso_syntax:ann().
ann() ->
    [{origin, system}].

args(State) ->
    [{typed, ann(), {id, ann(), Arg}, ArgT} || {Arg, ArgT, _} <- State#repl_state.vars].

-spec init() -> aeso_syntax:letfun().
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

-spec namespace(string(), list(aeso_syntax:decl())) -> aeso_syntax:decl().
namespace(Name, Body) ->
    {namespace, ann(), {con, ann(), Name}, Body}.

-spec type_def(string(), [aeso_syntax:tvar()], aeso_syntax:typedef()) -> aeso_syntax:decl().
type_def(Name, Args, Def) ->
    {type_def, ann(), {id, ann(), Name}, Args, Def}.

-spec using(string()) -> aeso_syntax:decl().
using(Namespace) ->
    {using, ann(), {con, ann(), Namespace}, none, none}.

typedef_namespaces(#repl_state{typedefs = Typedefs}) ->
    Namespaces =
        [ namespace(Ns, [type_def(Name, Args, Def)])
          || {Ns, Name, Args, Def} <- Typedefs
        ],
    lists:reverse(Namespaces).

type_scope_usings(#repl_state{type_scope = TypeScope}) ->
    [using(Namespace) || {_, {Namespace, _}} <- TypeScope].

mock_contract_ast(Body, State) ->
    Ns = typedef_namespaces(State),
    Ta = type_scope_usings(State),
    Con = contract(Ta ++ Body),
    Ns ++ [Con].

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
    CallFun = function([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body),
    mock_contract_ast([CallFun], State).

letfun_contract(FName, Args, FBody, State) ->
    FunDef = {letfun, [entrypoint| ann()], FName, [{tuple, ann(), args(State)}|Args], {id, ann(), "_"}, FBody},
    CallFun = function([entrypoint, payable, stateful], ?USER_INPUT, [], FName),
    mock_contract_ast([FunDef, CallFun], State).

-spec letval_contract(aeso_syntax:pattern(), [string()], aeso_syntax:expr(), repl_state()) -> aeso_syntax:ast().
letval_contract(Pattern, Vars, Expr, State) ->
    Let = {letval, ann(), Pattern, Expr},
    Ret = {tuple, ann(), [{string, ann(), ?LETVAL_INDICATOR}] ++ [{id, ann(), Var} || Var <- Vars]},
    Body = {block, ann(), [Let, Ret]},
    mock_contract_ast([function([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body)], State).

typedef_contract(Name, Args, Def, State) ->
    TDef = {type_def, ann(), {id, ann(), Name}, Args, Def},
    mock_contract_ast([TDef], State).

type_unfold_contract(State = #repl_state{type_scope = TypeScope}) ->
    type_unfold_contract(TypeScope, State).
type_unfold_contract(Types, State) ->
    Aliases =
        [ begin
              ArgsT = [{tvar, ann(), "t" ++ integer_to_list(N)} || N <- lists:seq(1, ArgsN)],
              Target = case ArgsN of
                        0 -> {qid, ann(), [Ns, Type]};
                        _ -> {app_t, ann(), {qid, ann(), [Ns, Type]}, ArgsT}
                    end,
              type_def(Type, ArgsT, {alias_t, Target})
          end
         || {Type, {Ns, ArgsN}} <- Types
        ],
    typedef_namespaces(State) ++ [contract(Aliases)].
