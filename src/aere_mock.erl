%-------------------------------------------------------------------
%% @doc Set of predefined ASTs for defining REPL's virtual contract
%% @end
%%%-------------------------------------------------------------------

-module(aere_mock).

-include("aere_repl.hrl").

-export([ chained_query_contract/2
        , chained_initial_contract/3
        , simple_query_contract/2
        , letdef_provider/3, letdef_provider/4
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


%% Attributes entrypoint Name(Args) = Body
entrypoint(Name, Args, Body) ->
    entrypoint(Name, Args, Body, []).
entrypoint(Name, Args, Body, full) ->
    entrypoint(Name, Args, Body, [payable, stateful]);
entrypoint(Name, Args, Body, Attrs) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , Args
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
simple_query_contract( State = #repl_state{ let_defs = LetDefs
                                          , local_funs = LocFuns
                                          , tracked_contracts = TrackedCons
                                          }, Expr) ->
    Con = contract(
            [val_entrypoint( ?USER_INPUT
                           , with_value_refs(TrackedCons, LetDefs, Expr), full)]),
    prelude(State) ++ [with_letfuns(TrackedCons, LetDefs, LocFuns, Con)].


%% Contract that evals expression and chains state if it makes sense
chained_query_contract(State = #repl_state
                       { let_defs = LetDefs
                       , local_funs = LocFuns
                       , tracked_contracts = TrackedCons
                       , user_contract_state_type = StType
                       }, Expr) ->
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Query = contract(state_init(State) ++
                         [ val_entrypoint(?USER_INPUT, with_value_refs(TrackedCons, LetDefs, Expr), full)
              , val_entrypoint(?GET_STATE, {id, ann(), "state"})
              ]),
    prelude(State) ++ [Prev, with_letfuns(TrackedCons, LetDefs, LocFuns, Query)].


%% Contract that initializes state chaining
chained_initial_contract(State = #repl_state{ let_defs = LetDefs
                                            , local_funs = LocFuns
                                            , tracked_contracts = TrackedCons
                                            }, Expr, Type) ->
    Con = with_letfuns(TrackedCons, LetDefs, LocFuns,
                       contract([ typedef("state", Type)
                                , val_entrypoint("init", with_value_refs(TrackedCons, LetDefs, Expr))
                                , val_entrypoint(?GET_STATE, {id, ann(), "state"})
                                ])),
    prelude(State) ++ [Con].


%% Contract that exposes value/function via entrypoint.
%% Not stateful nor payable, but remembers already defined values.
letdef_provider(State = #repl_state{ let_defs = LetDefs
                                   , local_funs = LocFuns
                                   , tracked_contracts = TrackedCons
                                   , user_contract_state_type = StType
                                   }, Name, Body) ->
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Con = contract(?LETDEF_PROVIDER(Name)
                  , [val_entrypoint( ?LETVAL_GETTER(Name)
                                   , with_value_refs(TrackedCons, LetDefs, Body))|state_init(State)]
                  ),
    prelude(State) ++ [Prev, with_letfuns(TrackedCons, LetDefs, LocFuns, Con)].
letdef_provider(State = #repl_state{ let_defs = LetDefs
                                   , local_funs = LocFuns
                                   , tracked_contracts = TrackedCons
                                   , user_contract_state_type = StType
                                   }, Name, Args, Body) ->
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, [], StType)]),
    Con = contract(?LETDEF_PROVIDER(Name)
                    , [entrypoint( Name
                                 , Args, with_value_refs(TrackedCons, LetDefs, Body))|state_init(State)]
                  ),
    prelude(State) ++ [Prev, with_letfuns(TrackedCons, LetDefs, LocFuns, Con)].


%% Declarations of providers of values/fuctions
letdef_provider_decls(LetDefs, LocFun) ->
    Vals = [contract( ?LETDEF_PROVIDER_DECL(Name)
                    , [decl(?LETVAL_GETTER(Name), [], Type)])
            || {Name, {letval, _, Type}} <- LetDefs
           ],
    VLoc = [contract( ?LETDEF_PROVIDER_DECL(?ADD_OWNER(FName, Name))
                    , [decl(?LETVAL_GETTER(Name), [], Type)])
            || {FName, {letfun_local, _, _, LetDefsFun}} <- LocFun,
               {Name, {letval, _, Type}} <- LetDefsFun
           ],
    Funs = [contract(?LETDEF_PROVIDER_DECL(Name), [decl(Name, ArgTs, RetT)])
            || {Name, {letfun, _, _, {ArgTs, RetT}}} <- LetDefs
           ],
    Funs ++ Vals ++ VLoc.


%% let-statements that refer to value providers
letval_defs(LetDefs) ->
    letval_defs(LetDefs, #{}).
letval_defs(LetDefs, ProviderNaming) ->
    [ { letval, ann(), {id, ann(), Name}, Type
      , call_to_remote( ProviderRef
                      , maps:get(Name, ProviderNaming, ?LETDEF_PROVIDER_DECL(Name))
                      , ?LETVAL_GETTER(Name))}
      || {Name, {letval, ProviderRef, Type}} <- LetDefs].


%% Entrypoints that pass the calls to function providers
letfun_defs(LetDefs) ->
    [ entrypoint(Name, Args
                , call_to_remote( ProviderRef, ?LETDEF_PROVIDER_DECL(Name)
                                , Name
                                , [Id || {arg, _, Id, _} <- Args]))
      || {Name, {letfun, ProviderRef, Args, _}} <- LetDefs].


%% References to contracts with their types
contract_refs(Contracts) ->
    [ { letval, ann(), {id, ann(), Name}, ConName
      , {contract_pubkey, ann(), ConRef}}
      || {Name, {tracked_contract, ConRef, ConName, _}} <- Contracts
    ].


%% Entrypoints defined as repl-local. Stateful and payable, but with
%% some other limitations
local_letfun_defs(TrackedCons, Funs) ->
    [ begin
          NameMap = maps:from_list([{VName, ?LETDEF_PROVIDER_DECL(?ADD_OWNER(FName, VName))}
                                    || {VName, _} <- LetDefs]),
          entrypoint( FName, Args
                    , with_value_refs(TrackedCons, LetDefs, Body, NameMap), full)
      end
      || {FName, {letfun_local, Args, Body, LetDefs}} <- Funs
    ].


%% Declarations and includes to a contract
prelude(#repl_state{ include_ast = Includes
                   , tracked_contracts = TrackedCons
                   , let_defs = LetDefList
                   , local_funs = LocFuns
                   }) ->
    LetDefProviders = letdef_provider_decls(LetDefList, LocFuns),
    TrackedContractsDecls = [I || {_, {tracked_contract, _, _, I}} <- TrackedCons],
    Includes ++ TrackedContractsDecls ++ LetDefProviders.


%% Append functions' definitions to a body of a contract
with_letfuns(TrackedCons, LetDefList, LocFunList, {contract, Ann, Id, ConBody}) ->
    LetFuns = letfun_defs(LetDefList),
    LocalFuns = local_letfun_defs(TrackedCons, LocFunList),
    {contract, Ann, Id, LocalFuns ++ LetFuns ++ ConBody}.


%% Prefixes expression with letval definitions and contract references.
%% Takes map that may specify name of respective provider for letvals.
with_value_refs([], [], Expr) ->
    Expr;
with_value_refs(Contracts, LetDefs, Expr) ->
    {block, ann(), contract_refs(Contracts) ++ letval_defs(LetDefs) ++ [Expr]}.
with_value_refs([], [], Expr, _ProviderNaming) ->
    Expr;
with_value_refs(Contracts, LetDefs, Expr, ProviderNaming) ->
    {block, ann(), contract_refs(Contracts) ++
         letval_defs(LetDefs, ProviderNaming) ++ [Expr]}.
