-module(aere_response).

-export([convert_type/2]).

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
convert_type(_TEnv, {id, _, "oracle"}) ->
    word;
convert_type(_TEnv, {id, _, "oracle_quert"}) ->
    word;
convert_type(TEnv, {Id, _, Name})
  when Id =:= id; Id =:= qid ->
    {[], Cons} = maps:get(Name, TEnv),
    {variant_t, [ {CName, [convert_type(TEnv, T) || T <- CArgs]}
                 || {constr_t, _, {con, _, CName}, CArgs} <- Cons
                ]};
convert_type(TEnv, {app_t,_,{id,_,"list"},[T]}) ->
    {list, convert_type(TEnv, T)};
convert_type(TEnv, {app_t,_,{id,_,"option"},[T]}) ->
    {option, convert_type(TEnv, T)};
convert_type(TEnv, {app_t,_,{id,_,"map"},[K, V]}) ->
    {map, convert_type(TEnv, K), convert_type(TEnv, V)};
convert_type(TEnv, {app_t, _, {I, _, Name}, Args}) when I =:= id; I =:= qid ->
    {TArgs, Cons} = maps:get(Name, TEnv),
    Subst = maps:from_list(lists:zip(
                             [N || {tvar, _, N} <- TArgs],
                             Args
                            )),
    {variant_t, [ {CName, [ case T of
                                {tvar, _, N} -> convert_type(TEnv, maps:get(N, Subst));
                                _ -> convert_type(TEnv, T)
                            end
                           || T <- CArgs]}
                  || {constr_t, _, {con, _, CName}, CArgs} <- Cons
                ]}.
