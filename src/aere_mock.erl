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
ann() ->
    [{file, <<"REPL">>}].


%% contract Name =
%%   Body
contract(Body) ->
    contract(?MOCK_CONTRACT, Body).
contract(Name, Body) ->
    {contract, [payable, ann()], {con, ann(), Name},
     Body
    }.


%% type Name = Type
typedef(Name, Type) ->
    {type_def, ann(),
     {id, ann(), Name},
     [],
     {alias_t, Type}}.


%% Entrypoint without arguments
%% Attributes entrpoint Name() = Body
val_entrypoint(Name, Body) ->
    val_entrypoint(Name, Body, []).
val_entrypoint(Name, Body, full) ->
    val_entrypoint(Name, Body, [payable, stateful]);
val_entrypoint(Name, Body, Attrs) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , []
    , {id, ann(), "_"}
    , Body}.


%% Attributes entrypoint Name : Args -> Type
decl(Name, Args, Type) ->
    decl(Name, Args, Type, []).
decl(Name, Args, Type, full) ->
    decl(Name, Args, Type, [payable, stateful]);
decl(Name, Args, Type, Attrs) ->
    { fun_decl
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , {fun_t, ann(), [], Args, Type}
    }.


%% (Ref : RemoteName).FName(Args)
call_to_remote(Ref, RemoteName, FName) ->
    call_to_remote(Ref, RemoteName, FName, []).
call_to_remote(Ref, RemoteName, FName, Args) ->
    {app,[]
    , {proj, ann(), {typed, ann(), {contract_pubkey, ann(), Ref}, {con, ann(), RemoteName}}
      , {id, ann(), FName}}
    , Args}.


%% state and init() definitions
state_init(#repl_state{user_contract_state_type = {id, _, "unit"}}) ->
    [];
state_init(#repl_state{user_contract_state_type = {tuple_t, _, []}}) ->
    [];
state_init(#repl_state{user_contracts = []}) ->
    [];
state_init(#repl_state
           { user_contract_state_type = StType
           , user_contracts = [PrevRef|_]
           }) ->
    [ val_entrypoint("init", call_to_remote(PrevRef, ?PREV_CONTRACT, ?GET_STATE))
    , typedef("state", StType)].


with_auto_imports(State = #repl_state{include_files = Includes}, Expr) ->
    AutoImports =
        [ binary_to_list(AI)
          || AI <- aeso_parser:auto_imports(Expr)
        ],
    case aerepl:register_includes(State, AutoImports -- Includes) of
        {_, S} -> S;
        S = #repl_state{} -> S
    end.

with_letfun_auto_imports(State = #repl_state{letfuns = Letfuns}) ->
    FBodies =
          [ FBody
            || {_, {Funs, _, _}} <- Letfuns,
               {letfun, _, _, _, _, FBody} <- Funs
          ],
    lists:foldr(fun(FB, S) -> with_auto_imports(S, FB) end, State, FBodies).



%% Contract that evals Expr and does not chain state
simple_query_contract( State = #repl_state{ user_contract_state_type = StType
                                          }, Stmts) ->
    Body = {block, ann(), Stmts},
    State1 = with_auto_imports(State, Body),
    State2 = with_letfun_auto_imports(State1),
    Con = contract(
            letfun_defs(State2) ++
                [ typedef("state", StType)
                , val_entrypoint( ?USER_INPUT
                                , with_value_refs(State2, Body), full)]),
    prelude(State2) ++ [Con].


%% Contract that evals expression and chains state if it makes sense
chained_query_contract(State = #repl_state
                       { user_contract_state_type = StType
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
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Query = contract(state_init(State2) ++ letfun_defs(State2) ++ typedefs(State2) ++
                         [ val_entrypoint(?USER_INPUT, with_value_refs(State2, Body), full)
                         , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                         ]),
    prelude(State2) ++ [Prev, Query].

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
chained_initial_contract(State, Stmts, Type) ->
    Body = {block, ann(), Stmts},
    Con = contract([ typedef("state", Type)
                   , val_entrypoint("init", with_value_refs(State, Body))
                   , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                   ]),
    prelude(State) ++ [Con].


%% Contract that exposes value via entrypoint.
%% Not stateful nor payable, but remembers already defined values.
letval_provider(State = #repl_state{ user_contract_state_type = StType
                                   }, Name, Body) ->
    State1 = with_auto_imports(State, [Body]),
    State2 = with_letfun_auto_imports(State1),
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Con = contract(?LETVAL_PROVIDER(Name)
                  , [val_entrypoint( ?LETVAL_GETTER(Name)
                                   , with_value_refs(State2, Body))|state_init(State)]
                   ++ letfun_defs(State2)
                  ),
    prelude(State2) ++ [Prev, Con].


%% Declarations of providers of values
letval_provider_decls(#repl_state{letvals = Letvals}) ->
    [contract( ?LETVAL_PROVIDER_DECL(Name)
             , [decl(?LETVAL_GETTER(Name), [], Type)])
     || {{Name, _}, {_, Type}} <- Letvals
    ].


%% let-statements that refer to value providers
letval_defs(#repl_state{letvals = LetVals}) ->
    [ { letval, ann(), Pat
      , call_to_remote( ProvRef
                      , ?LETVAL_PROVIDER_DECL(Provider)
                      , ?LETVAL_GETTER(Provider))
      }
      || {{Provider, ProvRef}, {Pat, _}} <- lists:reverse(LetVals)].


letfun_defs(State = #repl_state{ letfuns = LetFuns
                               }) ->
    [ { block, ann()
      , [ case F of
              {fun_decl, _, _, _} -> F;
              {letfun, A, N, Args, RT, Body} ->
                  {Cons1, LetVals1} =
                      begin
                          UsedNames = [AN || Arg <- Args,
                                             AN <- aere_sophia:get_pat_ids(Arg)
                                      ],
                          aerepl:remove_references(UsedNames, Cons, LetVals)
                      end,
                  State1 = State#repl_state{tracked_contracts = Cons1, letvals = LetVals1},
                  {letfun, A, N, Args, RT, with_value_refs(State1, Body)}
          end
          || F <- Funs
        ]}
      || {_, {Funs, Cons, LetVals}} <- LetFuns
    ].

%% References to contracts with their types
contract_refs(#repl_state{tracked_contracts = Contracts}) ->
    [ { letval, ann(), {id, ann(), Name}
      , {typed, ann(), {contract_pubkey, ann(), ConRef}, ConName}}
      || {Name, {tracked_contract, ConRef, {contract, _, ConName, _}}} <- Contracts
    ].


%% Namespaces that encapsulate actual typedef definitions
typedef_namespaces(#repl_state{typedefs = Typedefs}) ->
    [ { namespace, ann(), {con, ann(), Namespace}
      , [{type_def, ann(), {id, NAnn, TName}, TArgs, TD}]}
      || {{qid, NAnn, [Namespace, TName]}, {TArgs, TD}} <- lists:reverse(Typedefs)
    ].
typedefs(#repl_state{typedefs = Typedefs}) ->
    [ {type_def, ann(), Name, TArgs, TD}
      || {Name = {id, _, _}, {TArgs, TD}} <- lists:reverse(Typedefs)
    ].


%% Declarations and includes to a contract
prelude(State = #repl_state{ tracked_contracts = TrackedCons
                           , include_ast = Includes
                           }) ->
    TDNamespaces = typedef_namespaces(State),
    LetvalProviders = letval_provider_decls(State),
    {TCUnique, _} = lists:foldr(
                      fun(C = {_, {_, _, {contract, _, TC, _}}}, {Acc, Set}) ->
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
with_value_refs(#repl_state{tracked_contracts = [], letvals = []}, Expr) ->
    Expr;
with_value_refs(State, Expr) ->
    {block, ann(), contract_refs(State) ++ letval_defs(State) ++ [Expr]}.
