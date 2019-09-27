-module(aere_mock).

-include("aere_repl.hrl").

-export([ ann/0, contract/1, contract/2, typedef/2, entrypoint/3, entrypoint/2
        , decl/3, decl/2, simple_query_contract/2, chained_query_contract/2
        , chained_initial_contract/3, with_prelude/2, with_letvals/2, letval_provider/3
        ]).


ann() ->
    [{file, <<"REPL">>}].


contract(Body) ->
    contract(?MOCK_CONTRACT, Body).
contract(Name, Body) ->
    {contract, ann(), {con, ann(), Name},
     Body
    }.


typedef(Name, Type) ->
    {type_def, ann(),
     {id, ann(), Name},
     [],
     {alias_t, Type}}.


entrypoint(Name, Body) ->
    entrypoint(Name, Body, []).
entrypoint(Name, Body, full) ->
    entrypoint(Name, Body, [payable, stateful]);
entrypoint(Name, Body, Attrs) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , []
    , {id, ann(), "_"}
    , Body}.


decl(Name, Type) ->
    decl(Name, Type, []).
decl(Name, Type, full) ->
    decl(Name, Type, [payable, stateful]);
decl(Name, Type, Attrs) ->
    { fun_decl
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , {fun_t, ann(), [], [], Type}
    }.


call_to_remote(Ref, FName) ->
    {app,[],
     {proj, aere_mock:ann(),
      {contract_pubkey, aere_mock:ann(), Ref},
      {id, aere_mock:ann(), FName}
     }, []}.


simple_query_contract(State, Expr) ->
    with_prelude( State
                , contract([entrypoint(?USER_INPUT, with_letvals(State, Expr), full)])).


chained_query_contract(
  State = #repl_state{user_contract_state_type = {id, _, "unit"}}, Expr) ->
    simple_query_contract(State, Expr);
chained_query_contract(
  State = #repl_state{user_contract_state_type = {tuple_t, _, []}}, Expr) ->
    simple_query_contract(State, Expr);
chained_query_contract(
  State = #repl_state{user_contracts = []}, Expr) ->
    simple_query_contract(State, Expr);
chained_query_contract(State = #repl_state
                       { include_ast = Includes
                       , tracked_contracts = TContracts
                       , user_contract_state_type = StType
                       , let_defs = LetDefs
                       , user_contracts = [PrevRef|_]
                       }, Expr) ->
    Prev = contract(?PREV_CONTRACT, [decl(?GET_STATE, StType)]),
    Query = contract(
              [ typedef("state", StType)
              , entrypoint("init", call_to_remote(PrevRef, ?GET_STATE))
              , entrypoint(?USER_INPUT, with_letvals(State, Expr), full)
              , entrypoint(?GET_STATE, {id, ann(), "state"})
              ]),
    LetValProviders = letval_provider_decls(State),
    Includes ++ LetValProviders ++ [Prev, Query].


chained_initial_contract(State, Expr, Type) ->
    with_prelude(State, contract([ typedef("state", Type)
                                 , entrypoint("init", Expr)
                                 , entrypoint(?GET_STATE, {id, ann(), "state"})
                                 ])).


letval_provider(State, Name, Body) ->
    with_prelude(State, contract(?LETVAL_PROVIDER(Name)
                                , [entrypoint(Name, with_letvals(State, Body))])).


letval_provider_decls(#repl_state{let_defs = LetDefMap}) ->
    [contract(?LETVAL_PROVIDER(Name), [decl(Name, Type)])
     || {Name, {letval, _, Type}} <- LetDefMap
    ].


letval_defs(#repl_state{let_defs = LetDefs}) ->
    [ { letval, ann(), {id, ann(), Name}, Type
      , call_to_remote(ProviderRef, Name)}
      || {Name, {letval, ProviderRef, Type}} <- LetDefs].


letfun_defs(#repl_state{let_defs = LetDefs}) ->
    [ { letfun
      , ann() ++ [{entrypoint, true}, {payable, true}, {stateful, true}]
      , {id, ann(), Name}
      , Args
      , {id, ann(), "_"}
      , Body}
      || {Name, {letfun, Args, Body}} <- LetDefs].


with_prelude(State = #repl_state{include_ast = Includes},
             {contract, Ann, Id,
              ConBody
             }
            ) ->
    LetFuns = letfun_defs(State),
    LetValProviders = letval_provider_decls(State),
    Includes ++ LetValProviders ++ [{contract, Ann, Id, LetFuns ++ ConBody}].


with_letvals(State, Expr) ->
    {block, ann(), letval_defs(State) ++ [Expr]}.
