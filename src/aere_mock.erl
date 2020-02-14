%-------------------------------------------------------------------
%% @doc Set of predefined ASTs for defining REPL's virtual contract
%% @end
%%%-------------------------------------------------------------------

-module(aere_mock).

-include("aere_repl.hrl").

-export([ chained_query_contract/2
        , chained_initial_contract/3
        , simple_query_contract/2
        , letval_provider/3
        , unshadow_names/3
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
    {contract, ann(), {con, ann(), Name},
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


%% Contract that evals Expr and does not chain state
simple_query_contract( State = #repl_state{ letvals = LetDefs
                                          , tracked_contracts = TrackedCons
                                          }, Expr) ->
    Con = contract(
            [val_entrypoint( ?USER_INPUT
                           , with_value_refs(TrackedCons, LetDefs, Expr), full)]),
    prelude(State) ++ [Con].


%% Contract that evals expression and chains state if it makes sense
chained_query_contract(State = #repl_state
                       { letvals = LetVals
                       , letfuns = LetFuns
                       , include_files = Includes
                       , tracked_contracts = TrackedCons
                       , user_contract_state_type = StType
                       }, Stmts) ->
    Body = {block, ann(), Stmts},
    AutoImports = [binary_to_list(AI) || AI <- aeso_parser:auto_imports(Body)],
    WithAuto = case aerepl:register_includes(State, AutoImports -- Includes) of
                   {success, _, S} -> S;
                   {success, S} -> S;
                   {error, Msg} -> throw({error, "While importing auto import: " ++ Msg})
               end,
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Query = contract(state_init(State) ++ letfun_defs(LetFuns) ++
                         [ val_entrypoint(?USER_INPUT, with_value_refs(TrackedCons, LetVals, Body), full)
                         , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                         ]),
    prelude(WithAuto) ++ [Prev, Query].


%% Contract that initializes state chaining
chained_initial_contract(State = #repl_state{ letvals = LetDefs
                                            , tracked_contracts = TrackedCons
                                            }, Expr, Type) ->
    Con = contract([ typedef("state", Type)
                   , val_entrypoint("init", with_value_refs(TrackedCons, LetDefs, Expr))
                   , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                   ]),
    prelude(State) ++ [Con].


%% Contract that exposes value via entrypoint.
%% Not stateful nor payable, but remembers already defined values.
letval_provider(State = #repl_state{ letvals = LetDefs
                                   , tracked_contracts = TrackedCons
                                   , user_contract_state_type = StType
                                   }, Name, Body) ->
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Con = contract(?LETVAL_PROVIDER(Name)
                  , [val_entrypoint( ?LETVAL_GETTER(Name)
                                   , with_value_refs(TrackedCons, LetDefs, Body))|state_init(State)]
                  ),
    prelude(State) ++ [Prev, Con].


%% Declarations of providers of values
letdef_provider_decls(LetDefs) ->
    [contract( ?LETVAL_PROVIDER_DECL(Name)
             , [decl(?LETVAL_GETTER(Name), [], Type)])
     || {{Name, _}, {_, Type}} <- LetDefs
    ].


%% let-statements that refer to value providers
letval_defs(LetVals) ->
    [ { letval, ann(), Pat
      , { typed, ann(), call_to_remote( ProvRef
                                      , ?LETVAL_PROVIDER_DECL(Provider)
                                      , ?LETVAL_GETTER(Provider))
        , Type}}
      || {{Provider, ProvRef}, {Pat, Type}} <- lists:reverse(LetVals)].

unshadow_names(Names, Cons, LetVals) ->
    { [ case lists:member(CName, Names) of
            true -> {CName, {shadowed_contract, ConRef, ConName, I}};
            false -> C
        end
        || C = {CName, {_, ConRef, ConName, I}} <- Cons
      ]
    , [ {{Provider, ProvRef}, {NewPat, Type}}   %% removing letvals shadowed by rec and args
        || {{Provider, ProvRef}, {Pat, Type}} <- LetVals,
           NewPat <- [lists:foldl(
                        fun(V, P) -> aere_sophia:replace_var(P, V, "_")
                        end, Pat, Names)],
           lists:any(fun({id, _, "_"}) -> false; %% If everything is removed, why even consider it?
                        (_) -> true
                     end, aere_sophia:get_pat_ids(NewPat))
      ]
    }.

letfun_defs(LetFuns) ->
    [ {block, ann(), [ case F of
                           {fundecl, _, _, _} -> F;
                           {letfun, A, N, Args, RT, Body} ->
                               {Cons1, LetVals1} =
                                   begin
                                       UsedNames = [AN || Arg <- Args,
                                                          AN <- aere_sophia:get_pat_ids(Arg)
                                                   ],
                                      unshadow_names(UsedNames, Cons, LetVals)
                                   end,
                               {letfun, A, N, Args, RT, with_value_refs(Cons1, LetVals1, Body)}
                       end
                      || F <- Funs
                     ]}
     || {_, {Funs, Cons, LetVals}} <- LetFuns
    ].

%% References to contracts with their types
contract_refs(Contracts) ->
    [ { letval, ann(), {id, ann(), Name}
      , {typed, ann(), {contract_pubkey, ann(), ConRef}, ConName}}
      || {Name, {tracked_contract, ConRef, ConName, _}} <- Contracts
    ].

%% Declarations and includes to a contract
prelude(#repl_state{ include_ast = Includes
                   , tracked_contracts = TrackedCons
                   , letvals = LetDefList
                   }) ->
    LetDefProviders = letdef_provider_decls(LetDefList),
    TrackedContractsDecls = [I || {_, {_Status, _, _, I}} <- TrackedCons],
    Includes ++ TrackedContractsDecls ++ LetDefProviders.


%% Prefixes expression with letval definitions and contract references.
%% Takes map that may specify name of respective provider for letvals.
with_value_refs([], [], Expr) ->
    Expr;
with_value_refs(Contracts, LetDefs, Expr) ->
    {block, ann(), contract_refs(Contracts) ++ letval_defs(LetDefs) ++ [Expr]}.
