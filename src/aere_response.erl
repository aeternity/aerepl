-module(aere_response).

-export([convert_type/2, pp_response/1]).

-import(prettypr, [text/1, sep/1, above/2, beside/2, nest/2, empty/0]).


convert_type(_TEnv, {tvar, _, _}) ->
    word;
convert_type(_TEnv, {id, _, "int"}) ->
    signed_word;
convert_type(_TEnv, {id, _, "bool"}) ->
    bool;
convert_type(_TEnv, {id, _, "bits"}) ->
    word;
convert_type(_TEnv, {id, _, "string"}) ->
    string;
convert_type(_TEnv, {id, _, "address"}) ->
    word;
convert_type(_TEnv, {id, _, "hash"}) ->
    word;
convert_type(_TEnv, {id, _, "unit"}) ->
    {tuple, []};
convert_type(TEnv, {tuple_t, _, Ts}) ->
    {tuple, [convert_type(TEnv, T) || T <- Ts]};
convert_type(_TEnv, {id, _, "signature"}) ->
    word;
convert_type(_TEnv, {qid, _, ["Chain", "ttl"]}) ->
    {variant_t, [{{variant, "RelativeTTL"}, [word]}, {{variant, "FixedTTL"}, [word]}]};
convert_type(TEnv, {Id, _, Name})
  when Id =:= id; Id =:= qid ->
    case maps:get(Name, TEnv) of
        {variant, [], Cons} ->
            {variant_t, [ {{variant, CName}, [convert_type(TEnv, T) || T <- CArgs]}
                          || {constr_t, _, {con, _, CName}, CArgs} <- Cons
                        ]};
        {record, [], Fields} ->
            {tuple, [convert_type(TEnv, T) || {field_t, _, _, T} <- Fields]}
    end;
convert_type(TEnv, {record_t, Fields}) ->
    {tuple, [convert_type(TEnv, T) || {field_t, _, _, T} <- Fields]};
convert_type(TEnv, {app_t,_,{id,_,"list"},[T]}) ->
    {list, convert_type(TEnv, T)};
convert_type(TEnv, {app_t,_,{id,_,"option"},[T]}) ->
    {option, convert_type(TEnv, T)};
convert_type(_TEnv, {app_t,_,{id, _, "oracle"}, _}) ->
    word;
convert_type(_TEnv, {app_t,_,{id, _, "oracle_query"}, _}) ->
    word;
convert_type(TEnv, {app_t,_,{id,_,"map"},[K, V]}) ->
    {map, convert_type(TEnv, K), convert_type(TEnv, V)};
convert_type(TEnv, {app_t, _, {I, _, Name}, Args}) when I =:= id; I =:= qid ->
    case maps:get(Name, TEnv) of
        {variant, TArgs, Cons} ->
            Subst = maps:from_list(lists:zip([N || {tvar, _, N} <- TArgs], Args)),
            {variant_t, [ {{variant, CName}
                          , [ convert_type(TEnv, apply_subst(Subst, T)) || T <- CArgs]}
                          || {constr_t, _, {con, _, CName}, CArgs} <- Cons
                        ]};
        {record, TArgs, Fields} ->
            Subst = maps:from_list(lists:zip([N || {tvar, _, N} <- TArgs], Args)),
            {tuple, [convert_type(TEnv, apply_subst(Subst, T)) || {field_t, _, _, T} <- Fields]}
    end.


-define(aere_print_options, aere_print_options).

options() ->
    case get(?aere_print_options) of
        undefined -> [];
        Opts      -> Opts
    end.

option(Key, Default) ->
    proplists:get_value(Key, options(), Default).

show_generated() -> option(show_generated, false).

indent() -> option(indent, 2).

with_options(Options, Fun) ->
    put(?aere_print_options, Options),
    Res = Fun(),
    erase(?aere_print_options),
    Res.


punctuate(_Sep, [])      -> [];
punctuate(_Sep, [D])     -> [D];
punctuate(Sep, [D | Ds]) -> [beside(D, Sep) | punctuate(Sep, Ds)].


above([])       -> empty();
above([D])      -> D;
above([D | Ds]) -> lists:foldl(fun(X, Y) -> above(Y, X) end, D, Ds).

beside([])       -> empty();
beside([D])      -> D;
beside([D | Ds]) -> lists:foldl(fun(X, Y) -> beside(Y, X) end, D, Ds).


par(Ds) -> par(Ds, indent()).

par([], _) -> empty();
par(Ds, N) -> prettypr:par(Ds, N).

comma_brackets(Open, Close, Ds) ->
    beside([ text(Open)
           , par(punctuate(text(","), Ds), 0)
           , text(Close)]).

pp_response(I) when is_integer(I) ->
    text(integer_to_list(I));
pp_response(true) ->
    text("true");
pp_response(false) ->
    text("false");
pp_response(S) when is_binary(S) ->
    beside([text("\""), text(binary_to_list(S)), text("\"")]);
pp_response(L) when is_list(L) ->
    comma_brackets("[", "]", [pp_response(X) || X <- L]);
pp_response(M) when is_map(M) ->
    comma_brackets
      ( "{"
      , "}"
      , [ beside(
            [ text("[")
            , pp_response(K)
            , text("] = ")
            , pp_response(V)
            ])
         || {K, V} <- maps:to_list(M)
        ]
      );
pp_response(none) ->
    text("None");
pp_response({some, X}) ->
    beside([text("Some("), pp_response(X), text(")")]);
pp_response({variant, Cons}) ->
    text(Cons);
pp_response(T) when is_tuple(T) ->
    case tuple_to_list(T) of
        [{variant, Cons}|Args] ->
            beside(
              [ text(Cons)
              , comma_brackets("(", ")", [pp_response(A) || A <- Args])
              ]
             );
        TL -> comma_brackets("(", ")", [pp_response(X) || X <- TL])
    end.

apply_subst(Subst, {fun_t, Ann, NArgs, Args, Res}) ->
    {fun_t, Ann, [apply_subst(Subst, NA) || NA <- NArgs], [apply_subst(Subst, A) || A <- Args], apply_subst(Subst, Res)};
apply_subst(Subst, {app_t, Ann, T, Args}) ->
    {app_t, Ann, T, [apply_subst(Subst, A) || A <- Args]};
apply_subst(Subst, {tuple_t, Ann, [Types]}) ->
    {tuple_t, Ann, [apply_subst(Subst, T) || T <- Types]};
apply_subst(Subst, {args_t, Ann, [Types]}) ->
    {args_t, Ann, [apply_subst(Subst, T) || T <- Types]};
apply_subst(Subst, {tvar, _, Name}) ->
    maps:get(Name, Subst);
apply_subst(_, T) ->
    T.




