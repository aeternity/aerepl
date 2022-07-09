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
        , ann/0
        ]).

-type ast()      :: aeso_syntax:ast().
-type ann()      :: aeso_syntax:ann().
-type expr()     :: aeso_syntax:expr().
-type stmt()     :: aeso_syntax:stmt().
-type pat()      :: aeso_syntax:pat().
-type id()       :: aeso_syntax:id().
-type con()      :: aeso_syntax:con().
-type letfun()   :: aeso_syntax:letfun().
-type tvar()     :: aeso_syntax:tvar().
-type typedef()  :: aeso_syntax:typedef().
-type decl()     :: aeso_syntax:decl().

%%% --- Mocks --- %%%

%% Mock to validate and evaluate Sophia expression
-spec eval_contract(expr() | [stmt()], repl_state()) -> ast().
eval_contract(Body, State) ->
    CallFun = function([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body),
    mock_contract(State, [CallFun]).

%% Mock to validate and get bytecode of a function
-spec letfun_contract(string(), [pat()], expr(), repl_state()) -> ast().
letfun_contract(FName, Args, FBody, State) ->
    FunDef = {letfun, ann(), FName, [{tuple, ann(), args(State)}|Args], {id, ann(), "_"}, FBody},
    CallFun = function([entrypoint, payable, stateful], ?USER_INPUT, [], FName),
    mock_contract(State, [FunDef, CallFun]).

%% Mock to validate and compute value(s) of letval assignment
-spec letval_contract(pat(), [string()], expr(), repl_state()) -> ast().
letval_contract(Pattern, Vars, Expr, State) ->
    Let = {letval, ann(), Pattern, Expr},
    Ret = {tuple, ann(), [{string, ann(), ?LETVAL_INDICATOR}] ++ [{id, ann(), Var} || Var <- Vars]},
    Body = {block, ann(), [Let, Ret]},
    mock_contract(State, [function([entrypoint, payable, stateful], ?USER_INPUT, args(State), Body)]).

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
    Ast ++ [contract(?MOCK_CONTRACT, [function([entrypoint], ?USER_INPUT, [], {tuple, ann(), []})])].


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
mock_contract(State, Decls) ->
    with_state_ast(
      State,
      [contract(?MOCK_CONTRACT, with_state_decls(State, Decls))]).

%%% --- Sophia construction helpers --- %%%

%% Default annotation
-spec ann() -> ann().
ann() ->
    [{origin, system}].


-spec init() -> letfun().
init() ->
    function([entrypoint], "init", [], {tuple, ann(), []}).

-spec contract(string() | con(), list(decl())) -> decl().
contract(Name, Body) ->
    contract(contract_main, Name, Body).

-spec contract(contract_main | contract_interface, string() | con(), list(decl())) -> decl().
contract(ContractType, Name, Body) when is_list(Name) ->
    contract(ContractType, {con, ann(), Name}, Body);
contract(ContractType, Con, Body) ->
    {ContractType, [payable, ann()], Con, [], [init()|Body]}.

-spec namespace(string() | con(), list(decl())) -> decl().
namespace(Name, Body) when is_list(Name) ->
    namespace({con, ann(), Name}, Body);
namespace(Con, Body) ->
    {namespace, ann(), Con, Body}.

-spec type_def(string() | id(), [tvar()], typedef()) -> decl().
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

%% $Attrs entrypoint $Name($Args) = $Body
-spec function(ann(), string() | id(), [string() | pat()], expr() | [stmt()]) -> decl().
function(Attrs, Name, Args, Body) when is_list(Name) ->
    function(Attrs, {id, ann(), Name}, Args, Body);
function(Attrs, Id, Args, Body) when is_list(Body) ->
    function(Attrs, Id, Args, {block, ann(), Body});
function(Attrs, Id, Args, Body) ->
    { letfun
    , ann() ++ [A || A <- Attrs]
    , Id
    , Args
    , {id, ann(), "_"}
    , [{guarded, ann(), [], Body}]}.

