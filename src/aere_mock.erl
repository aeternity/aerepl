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
        , ast_fillup_contract/1
        , pat_as_decl/1
        , ann/0
        ]).

-type ast()          :: aeso_syntax:ast().
-type ann()          :: aeso_syntax:ann().
-type expr()         :: aeso_syntax:expr().
-type stmt()         :: aeso_syntax:stmt().
-type type()         :: aeso_syntax:type().
-type pat()          :: aeso_syntax:pat().
-type id()           :: aeso_syntax:id().
-type con()          :: aeso_syntax:con().
-type letfun()       :: aeso_syntax:letfun().
-type tvar()         :: aeso_syntax:tvar().
-type typedef()      :: aeso_syntax:typedef().
-type decl()         :: aeso_syntax:decl().
-type guarded_expr() :: aeso_syntax:guarded_expr().

%%% --- Mocks --- %%%

%% Mock to validate and evaluate Sophia expression
-spec eval_contract(expr() | [stmt()], repl_state()) -> ast().
eval_contract(Body, State) ->
    CallFun = function_e([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body),
    mock_contract(State, [CallFun]).

%% Mock to validate and get bytecode of a function
-spec letfun_contract(id(), [pat()], [guarded_expr()], repl_state()) -> ast().
letfun_contract(FName, Args, FBody, State) ->
    FunDef = function([entrypoint, payable, stateful], FName, Args, FBody),
    CallFun = function_e([entrypoint, payable, stateful], ?USER_INPUT, [], FName),
    mock_contract(State, [FunDef, CallFun]).

%% Mock to validate and compute value(s) of letval assignment
-spec letval_contract(pat(), [string()], expr(), repl_state()) -> ast().
letval_contract(Pattern, Vars, Expr, State) ->
    Let = {letval, ann(), Pattern, Expr},
    Ret = case Vars of
              [V] -> {id, ann(), V};
              _ -> {tuple, ann(), [{id, ann(), Var} || Var <- Vars]}
          end,
    Body = {block, ann(), [Let, Ret]},
    mock_contract(State, [function_e([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body)]).

%% Mock to validate type definition
-spec typedef_contract(string(), [tvar()], typedef(), repl_state()) -> ast().
typedef_contract(Name, Args, Def, State) ->
    TDef = {type_def, ann(), {id, ann(), Name}, Args, Def},
    mock_contract(State, [TDef]).

%% Mock for unfolding type aliases in type definitions
-spec type_unfold_contract(repl_state()) -> ast().
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
    mock_contract(State, Aliases).

%% Mock that just adds a sample main contract to the given AST
-spec ast_fillup_contract(ast()) -> ast().
ast_fillup_contract(Ast) ->
    Ast ++ [contract(?MOCK_CONTRACT, ?DEFAULT_CONTRACT_STATE_T, [function_e(?USER_INPUT, [], {tuple, ann(), []})])].

%% Puts a pattern as the value of a function. Used in collection of free variables
%% TODO This is a hack because used_ids requires decl; should be a feature of aesophia
pat_as_decl(Pat) ->
    function_e([], "_", [], Pat).

%%% --- Entities extracted from REPL state

-spec args(repl_state()) -> [pat()].
args(#repl_state{vars = Vars}) ->
    [{typed, ann(), {id, ann(), Arg}, ArgT} || {Arg, ArgT, _} <- Vars].

-spec typedef_namespaces(repl_state()) -> ast().
typedef_namespaces(#repl_state{typedefs = Typedefs}) ->
    Namespaces =
        [ namespace(Ns, [type_def(Name, Args, Def)])
          || {Ns, Name, Args, Def} <- Typedefs
        ],
    lists:reverse(Namespaces).

-spec includes(repl_state()) -> ast().
includes(#repl_state{included_code = IncFiles}) ->
    IncFiles.

-spec type_scope_usings(repl_state()) -> [decl()].
type_scope_usings(#repl_state{type_scope = TypeScope}) ->
    [using(Namespace) || {_, {Namespace, _}} <- TypeScope].

-spec with_state_decls(repl_state(), [decl()]) -> [decl()].
with_state_decls(State, Decls) ->
    Ta = type_scope_usings(State),
    Ta ++ Decls.

-spec with_state_ast(repl_state(), ast()) -> ast().
with_state_ast(State, Ast) ->
    Inc = includes(State),
    Ns = typedef_namespaces(State),
    Inc ++ Ns ++ Ast.

-spec mock_contract(repl_state(), [decl()]) -> ast().
mock_contract(State = #repl_state{contract_state = {StateT, _}}, Decls) ->
    with_state_ast(
      State,
      [contract(?MOCK_CONTRACT, StateT, with_state_decls(State, Decls))]).

%%% --- Sophia construction helpers --- %%%

%% Default annotation
-spec ann() -> ann().
ann() ->
    [{origin, system}].

-spec init() -> letfun().
init() ->
    Abort = {app, ann(), {id, ann(), "abort"}, [{string, ann(), "INIT NOT CALLABLE"}]},
    function_e([entrypoint], "init", [], [Abort]).

-spec state_typedef(type()) -> decl().
state_typedef(Type) ->
    type_def("state", [], {alias_t, Type}).

-spec contract(string() | con(), type(), list(decl())) -> decl().
contract(Name, ContractState, Body) ->
    contract(contract_main, ContractState, Name, Body).

-spec contract(contract_main | contract_interface, type(), string() | con(), list(decl())) -> decl().
contract(ContractType, StateT, Name, Body) when is_list(Name) ->
    contract(ContractType, StateT, {con, ann(), Name}, Body);
contract(ContractType, StateT, Con, Body) ->
    {ContractType, [payable, ann()], Con, [], [state_typedef(StateT), init() | Body]}.

-spec namespace(string() | con(), list(decl())) -> decl().
namespace(Name, Body) when is_list(Name) ->
    namespace({con, ann(), Name}, Body);
namespace(Con, Body) ->
    {namespace, ann(), Con, Body}.

-spec type_def(string() | id(), list(tvar()), typedef()) -> decl().
type_def(Name, Args, Def) when is_list(Name) ->
    type_def({id, ann(), Name}, Args, Def);
type_def(Id, Args, Def) ->
    {type_def, ann(), Id, Args, Def}.

%% using $Name
-spec using(string() | con()) -> decl().
using(Name) when is_list(Name) ->
    using({con, ann(), Name});
using(Con) ->
    {using, ann(), Con, none, none}.

%% Function constructor with unguarded body
-spec function_e(string() | id(), [string() | pat()], expr() | [stmt()]) -> decl().
function_e(Name, Args, Body) ->
    function_e(ann(), Name, Args, Body).

-spec function_e(ann(), string() | id(), [string() | pat()], expr() | [stmt()]) -> decl().
function_e(Attrs, Name, Args, Body) when is_list(Name) ->
    function_e(Attrs, {id, ann(), Name}, Args, Body);
function_e(Attrs, Id, Args, Body) when is_list(Body) ->
    function_e(Attrs, Id, Args, {block, ann(), Body});
function_e(Attrs, Id, Args, Body) ->
    function(Attrs, Id, Args, [{guarded, ann(), [], Body}]).

%% Function constructor with guarded body
-spec function(ann(), string() | id(), [string() | pat()], [guarded_expr()]) -> decl().
function(Attrs, Name, Args, Body) when is_list(Name) ->
    function(Attrs, {id, ann(), Name}, Args, Body);
function(Attrs, Id, Args, Clauses) ->
    { letfun
    , ann() ++ [A || A <- Attrs]
    , Id
    , Args
    , {id, ann(), "_"}
    , Clauses}.
