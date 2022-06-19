%-------------------------------------------------------------------
%% @doc Set of predefined ASTs for defining REPL's virtual contract
%% @end
%%%-------------------------------------------------------------------

-module(aere_mock).

-include("aere_macros.hrl").

-export([ mock_contract/1
        , ann/0
        ]).


%% Default annotation
-spec ann() -> aeso_syntax:ann().
ann() ->
    [{origin, system}].

%% contract Name =
%%   Body
-spec contract(list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(Body) ->
    contract(contract_main, ?MOCK_CONTRACT, Body).
-spec contract(contract_main | contract_interface, string(), list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(ContractType, Name, Body) ->
    {ContractType, [payable, ann()], {con, ann(), Name}, Body}.

%% Entrypoint without arguments
%% Attrs entrypoint Name() = Body
-spec val_entrypoint(string(), aeso_syntax:expr(), aeso_syntax:ann()) -> aeso_syntax:decl().

val_entrypoint(Name, Body, Attrs) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , []
    , {id, ann(), "_"}
    , [{guarded, ann(), [], Body}]}.


%% Contract that evals Expr and does not chain state
-spec mock_contract(list(aeso_syntax:stmt()))
                           -> aeso_syntax:decl().
mock_contract(Stmts) ->
    Body = {block, ann(), Stmts},
    contract([val_entrypoint(?USER_INPUT, Body, [payable, stateful])]).
