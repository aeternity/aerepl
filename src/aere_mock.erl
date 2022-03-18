%-------------------------------------------------------------------
%% @doc Set of predefined ASTs for defining REPL's virtual contract
%% @end
%%%-------------------------------------------------------------------

-module(aere_mock).

-include("aere_repl.hrl").
-include("aere_macros.hrl").

-export([ chained_query_contract/2
        , chained_initial_contract/3
        , simple_query_contract/2
        , letval_provider/3
        , ann/0
        ]).


%% Default annotation
-spec ann() -> aeso_syntax:ann().
ann() ->
    [{file, <<"REPL">>}].


%% contract Name =
%%   Body
-spec contract(list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(Body) ->
    contract(contract_main, ?MOCK_CONTRACT, Body).
-spec contract(contract_main | contract_interface, string(), list(aeso_syntax:decl())) -> aeso_syntax:decl().
contract(ContractType, Name, Body) ->
    {ContractType, [payable, ann()], {con, ann(), Name}, Body}.

%% type Name = Type
-spec type_alias(string(), aeso_syntax:type()) -> aeso_syntax:decl().
type_alias(Name, Type) ->
    {type_def, ann(),
     {id, ann(), Name},
     [],
     {alias_t, Type}}.


%% Entrypoint without arguments
%% Attributes entrpoint Name() = Body
-spec val_entrypoint(string(), aeso_syntax:expr()) -> aeso_syntax:decl().
val_entrypoint(Name, Body) ->
    val_entrypoint(Name, Body, []).
-spec val_entrypoint( string(), aeso_syntax:expr()
                    , full | list(payable | stateful))
                    -> aeso_syntax:decl().
val_entrypoint(Name, Body, full) ->
    val_entrypoint(Name, Body, [payable, stateful]);
val_entrypoint(Name, Body, Attrs) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , []
    , {id, ann(), "_"}
    , [{guarded, ann(), [], Body}]}.


%% Attributes entrypoint Name : Args -> Type
-spec entrypoint_decl(string(), list(aeso_syntax:pat()), aeso_syntax:type())
                     -> aeso_syntax:decl().
entrypoint_decl(Name, Args, Type) ->
    entrypoint_decl(Name, Args, Type, []).
entrypoint_decl(Name, Args, Type, full) ->
    entrypoint_decl(Name, Args, Type, [payable, stateful]);
entrypoint_decl(Name, Args, Type, Attrs) ->
    { fun_decl
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , {fun_t, ann(), [], Args, Type}
    }.


%% (Ref : RemoteName).FName(Args)
-spec call_to_remote(binary(), string(), string()) -> aeso_syntax:expr().
call_to_remote(Ref, RemoteName, FName) ->
    call_to_remote(Ref, RemoteName, FName, []).
-spec call_to_remote(binary(), string(), string(), list(aeso_syntax:arg_expr()))
                    -> aeso_syntax:expr().
call_to_remote(Ref, RemoteName, FName, Args) ->
    {app, ann()
    , {proj, ann(), {typed, ann(), {contract_pubkey, ann(), Ref}, {con, ann(), RemoteName}}
      , {id, ann(), FName}}
    , Args}.


%% state and init() definitions
-spec state_init(repl_state()) -> list(aeso_syntax:decl()).
state_init(#repl_state{user_contract_state_type = {id, _, "unit"}}) ->
    [];
state_init(#repl_state{user_contract_state_type = {tuple_t, _, []}}) ->
    [];
state_init(#repl_state{last_state_provider = undefined}) ->
    [];
state_init(#repl_state
           { user_contract_state_type = StType
           , last_state_provider = PrevRef
           }) ->
    [ val_entrypoint("init", call_to_remote(PrevRef, ?PREV_CONTRACT, ?GET_STATE))
    , type_alias("state", StType)].


-spec with_auto_imports(repl_state(), aeso_syntax:expr() | list(aeso_syntax:stmt())
                       ) -> aeso_syntax:expr() | list(aeso_syntax:stmt()).
with_auto_imports(State = #repl_state{include_files = Includes}, Expr) ->
    AutoImports =
        [ binary_to_list(AI)
          || AI <- aeso_parser:auto_imports(Expr)
        ],
    case aere_repl:register_includes(State, AutoImports -- Includes) of
        {_, S} -> S;
        S = #repl_state{} -> S
    end.

-spec with_letfun_auto_imports(repl_state()) -> repl_state().
with_letfun_auto_imports(State = #repl_state{letfuns = Letfuns}) ->
    FBodies =
          [ FBody
            || {_, {Funs, _, _}} <- Letfuns,
               {letfun, _, _, _, _, [{guarded, _, _, FBody}]} <- Funs
          ],
    lists:foldr(fun(FB, S) -> with_auto_imports(S, FB) end, State, FBodies).



%% Contract that evals Expr and does not chain state
-spec simple_query_contract(repl_state(), list(aeso_syntax:stmt()))
                           -> aeso_syntax:ast().
simple_query_contract(State = #repl_state{ user_contract_state_type = StType
                                         }, Stmts) ->
    Body = {block, ann(), Stmts},
    State1 = with_auto_imports(State, Body),
    State2 = with_letfun_auto_imports(State1),
    Con = contract(
            letfun_defs(State2) ++
                [ type_alias("state", StType)
                , val_entrypoint( ?USER_INPUT
                                , with_value_refs(State2, Body), full)]),
    prelude(State2) ++ [Con].


%% Contract that evals expression and chains state if it makes sense
-spec chained_query_contract(repl_state(), list(aeso_syntax:stmt()))
                            -> aeso_syntax:ast().
chained_query_contract(State = #repl_state
                       { user_contract_state_type = StType
                       , defined_contracts = DefinedContracts
                       , options = #options{call_value = CallValue}
                       }, Stmts) ->
    Stmts1 = if is_list(Stmts) -> Stmts;
                true -> [Stmts]
             end,
    Body = {block, ann(),
            case CallValue of
                0 -> Stmts1;
                _ -> with_token_refund(State, Stmts1)
            end},
    State1 = with_auto_imports(State, Body),
    State2 = with_letfun_auto_imports(State1),
    Prev = contract( contract_interface
                   , ?PREV_CONTRACT
                   , [entrypoint_decl(?GET_STATE, [], StType)]),
    Query = contract(state_init(State2) ++ letfun_defs(State2) ++ typedefs(State2) ++
                         [ val_entrypoint(?USER_INPUT, with_value_refs(State2, Body), full)
                         , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                         ]),
    DefinedContracts ++ prelude(State2) ++ [Prev, Query].

-spec with_token_refund(repl_state(), list(aeso_syntax:stmt())) -> list(aeso_syntax:stmt()).
with_token_refund(#repl_state{options = #options{call_value = 0}}, B) ->
    B; % didn't spend anything
with_token_refund(_, L) when is_list(L) ->
    [Last|Rest] = lists:reverse(L),
    Let = {letval, ann(), {id, ann(), "#RESULT_BACKUP"}, Last},
    lists:reverse
      ([ {id, ann(), "#RESULT_BACKUP"}
       , { app, ann(), {qid, ann(), ["Chain", "spend"]}
         , [ {qid, ann(), ["Call", "origin"]}
           , {qid, ann(), ["Contract", "balance"]}
           ]}
       , Let
       | Rest
       ]).


%% Contract that initializes state chaining
-spec chained_initial_contract(repl_state(), list(aeso_syntax:stmts()), aeso_syntax:type())
                              -> aeso_syntax:ast().
chained_initial_contract(State, Stmts, Type) ->
    Body = {block, ann(), Stmts},
    Con = contract([ type_alias("state", Type)
                   , val_entrypoint("init", with_value_refs(State, Body))
                   , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                   ]),
    prelude(State) ++ [Con].


%% Contract that exposes value via entrypoint.
%% Not stateful nor payable, but remembers already defined values.
-spec letval_provider(repl_state(), string(), aeso_syntax:expr())
                     -> aeso_syntax:ast().
letval_provider(State = #repl_state{ user_contract_state_type = StType
                                   , defined_contracts = DefinedContracts
                                   }, Name, Body) ->
    State1 = with_auto_imports(State, [Body]),
    State2 = with_letfun_auto_imports(State1),
    Prev = contract( contract_interface
                   , ?PREV_CONTRACT
                   , [entrypoint_decl(?GET_STATE, [], StType)]),
    Con = contract( contract_main
                  , ?LETVAL_PROVIDER(Name)
                  , [val_entrypoint( ?LETVAL_GETTER(Name)
                                   , with_value_refs(State2, Body)
                                   , [stateful] )|state_init(State)]
                   ++ letfun_defs(State2)
                  ),
    prelude(State2) ++ DefinedContracts ++ [Prev, Con].


%% Declarations of providers of values
-spec letval_provider_decls(repl_state()) -> list(aeso_syntax:decl()).
letval_provider_decls(#repl_state{letvals = Letvals}) ->
    [contract( contract_interface
             , ?LETVAL_PROVIDER_DECL(Name)
             , [entrypoint_decl(?LETVAL_GETTER(Name), [], Type)])
     || {{Name, _}, {_, Type}} <- Letvals
    ].


%% let-statements that refer to value providers
-spec letval_defs(repl_state()) -> list(aeso_syntax:stmt()).
letval_defs(#repl_state{letvals = LetVals}) ->
    [ { letval, ann(), Pat
      , call_to_remote( ProvRef
                      , ?LETVAL_PROVIDER_DECL(Provider)
                      , ?LETVAL_GETTER(Provider))
      }
      || {{Provider, ProvRef}, {Pat, _}} <- lists:reverse(LetVals)].

%% Definitions of functions defined by the user
-spec letfun_defs(repl_state()) -> list(aeso_syntax:decl()).
letfun_defs(State = #repl_state{ letfuns = LetFuns
                               }) ->
    [ { block, ann()
      , [ case F of
              {fun_decl, _, _, _} -> F;
              {letfun, A, N, Args, RT, [{guarded, _, _, Body}]} ->
                  {Cons1, LetVals1} =
                      begin
                          UsedNames = [AN || Arg <- Args,
                                             AN <- aere_sophia:get_pat_ids(Arg)
                                      ],
                          aere_repl:remove_references(UsedNames, Cons, LetVals)
                      end,
                  State1 = State#repl_state{tracked_contracts = Cons1, letvals = LetVals1},
                  {letfun, A, N, Args, RT, [{guarded, A, [], with_value_refs(State1, Body)}]}
          end
          || F <- Funs
        ]}
      || {_, {Funs, Cons, LetVals}} <- LetFuns
    ].

%% References to contracts with their types
-spec contract_refs(repl_state()) -> list(aeso_syntax:stmt()).
contract_refs(#repl_state{tracked_contracts = Contracts}) ->
    [ { letval, ann(), {id, ann(), Name}
      , {typed, ann(), {contract_pubkey, ann(), ConRef}, ConName}}
      || {Name, {tracked_contract, ConRef, {contract_interface, _, ConName, _}}} <- Contracts
    ].


%% Namespaces that encapsulate the actual typedef definitions
-spec typedef_namespaces(repl_state()) -> list(aeso_syntax:decl()).
typedef_namespaces(#repl_state{typedefs = Typedefs}) ->
    [ { namespace, ann(), {con, ann(), Namespace}
      , [{type_def, ann(), {id, NAnn, TName}, TArgs, TD}]}
      || {{qid, NAnn, [Namespace, TName]}, {TArgs, TD}} <- lists:reverse(Typedefs)
    ].

%% TODO for which fuck is it here?
-spec typedefs(repl_state()) -> list(aeso_syntax:decl()).
typedefs(#repl_state{typedefs = Typedefs}) ->
    [ {type_def, ann(), Name, TArgs, TD}
      || {Name = {id, _, _}, {TArgs, TD}} <- lists:reverse(Typedefs)
    ].


%% Declarations and includes to a contract
-spec prelude(repl_state()) -> list(aeso_syntax:decl()).
prelude(State = #repl_state{ tracked_contracts = TrackedCons
                           , include_ast = Includes
                           }) ->
    TDNamespaces = typedef_namespaces(State),
    LetvalProviders = letval_provider_decls(State),
    {TCUnique, _} = lists:foldr(
                      fun(C = {_, {_, _, {contract_interface, _, TC, _}}}, {Acc, Set}) ->
                              { case sets:is_element(TC, Set) of
                                    true -> Acc;
                                    false -> [C|Acc]
                                end
                              , sets:add_element(TC, Set)
                              }
                      end, {[], sets:new()}, TrackedCons
                     ),
    TrackedContractsDecls = [I || {_, {_Status, _, I}} <- TCUnique],
    Includes ++ TDNamespaces ++ TrackedContractsDecls ++ LetvalProviders.


%% Prefixes expression with letval definitions and contract references.
%% Takes map that may specify name of respective provider for letvals.
-spec with_value_refs(repl_state(), aeso_syntax:expr()) -> aeso_syntax:expr().
with_value_refs(#repl_state{tracked_contracts = [], letvals = []}, Expr) ->
    Expr;
with_value_refs(State, Expr) ->
    {block, ann(), contract_refs(State) ++ letval_defs(State) ++ [Expr]}.
