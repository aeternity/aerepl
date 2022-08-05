-module(aere_response).

-export([convert_type/2, pp_response/1, encode/1, decode/1, decode/2]).

-import(prettypr, [text/1, sep/1, above/2, beside/2, nest/2, empty/0]).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

convert_type(_TEnv, {tvar, _, _}) ->
    word;
convert_type(_TEnv, {id, _, "int"}) ->
    signed_word;
convert_type(_TEnv, {id, _, "bool"}) ->
    bool;
convert_type(_TEnv, {bytes_t, _, _}) ->
    word;
convert_type(_TEnv, {id, _, "bits"}) ->
    word;
convert_type(_TEnv, {id, _, "string"}) ->
    string;
convert_type(_TEnv, {id, _, "address"}) ->
    word;
convert_type(_TEnv, {id, _, "hash"}) ->
    word;
convert_type(_TEnv, {con, _, _}) ->
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
    end;
convert_type(_TEnv, {fun_t, _, _, _, _}) ->
    word.

-define(aere_print_options, aere_print_options).

punctuate(_Sep, [])      -> [];
punctuate(_Sep, [D])     -> [D];
punctuate(Sep, [D | Ds]) -> [beside(D, Sep) | punctuate(Sep, Ds)].

-spec beside([prettypr:document()]) -> prettypr:document().
beside([D])      -> D;
beside([D | Ds]) -> lists:foldl(fun(X, Y) -> beside(Y, X) end, D, Ds).

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
      ( "{", "}"
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
pp_response({revert, Msg}) ->
    text("REVERT: " ++ binary_to_list(Msg));
pp_response({address, Addr}) ->
    text(binary_to_list(aeser_api_encoder:encode(peer_pubkey, <<Addr:256>>)));
pp_response({bytes, Bs}) ->
    beside([text("#")|
            [text(io_lib:format("~.16b", [X])) || X <- binary_to_list(Bs)]
           ]);
pp_response({contract, Addr}) ->
    text(binary_to_list(aeser_api_encoder:encode(contract_pubkey, <<Addr:256>>)));
pp_response({oracle, Addr}) ->
    text(io_lib:format("~p", [Addr]));
pp_response({oracle_query, Addr}) ->
    text(io_lib:format("~p", [Addr]));
pp_response({channel, Addr}) ->
    text(io_lib:format("~p", [Addr]));
pp_response({bits, Addr}) ->
    text(io_lib:format("~p", [Addr]));
pp_response(T) when is_tuple(T) ->
    case tuple_to_list(T) of
        [{variant, Cons}|Args] ->
            beside(
              [ text(Cons)
              , comma_brackets("(", ")", [pp_response(A) || A <- Args])
              ]
             );
        TL -> comma_brackets("(", ")", [pp_response(X) || X <- TL])
    end;
pp_response(R) ->
    text(io_lib:format("~p", [R])).


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

encode({bits, Term}) when is_integer(Term) -> aeb_fate_data:make_bits(Term);
encode({address, B}) when is_binary(B)  -> aeb_fate_data:make_address(B);
encode({address, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_address(B);
encode({address, S}) when is_list(S)  ->
    aeb_fate_data:make_address(encode_address(account_pubkey, S));
encode({hash, H}) when is_binary(H)  -> aeb_fate_data:make_hash(H);
encode({hash, H}) when is_list(H)  -> aeb_fate_data:make_hash(base64:decode(H));
encode({signature, S}) when is_binary(S)  -> aeb_fate_data:make_signature(S);
encode({signature, S}) when is_list(S)  ->
    aeb_fate_data:make_signature(encode_address(signature, S));
encode({bytes, B}) when is_binary(B) -> aeb_fate_data:make_bytes(B);
encode({contract, B}) when is_binary(B)  -> aeb_fate_data:make_contract(B);
encode({contract, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_contract(B);
encode({contract, S}) when is_list(S)  ->
    aeb_fate_data:make_contract(encode_address(contract_pubkey, S));
encode({oracle, B}) when is_binary(B)  -> aeb_fate_data:make_oracle(B);
encode({oracle, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_oracle(B);
encode({oracle_query, B}) when is_binary(B)  -> aeb_fate_data:make_oracle_query(B);
encode({oracle_query, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_oracle_query(B);
encode({oracle, S}) when is_list(S)  ->
   aeb_fate_data:make_oracle(encode_address(oracle_pubkey, S));
encode({channel, B}) when is_binary(B)  -> aeb_fate_data:make_channel(B);
encode({channel, I}) when is_integer(I)  -> B = <<I:256>>, aeb_fate_data:make_channel(B);
encode({channel, S}) when is_list(S)  ->
    aeb_fate_data:make_channel(encode_address(channel, S));
encode({variant, Arities, Tag, Values}) ->
    aeb_fate_data:make_variant(Arities, Tag, list_to_tuple([encode(V) || V <- tuple_to_list(Values)]));
encode(none)      -> aeb_fate_data:make_variant([0, 1], 0, {});
encode({some, X}) -> aeb_fate_data:make_variant([0, 1], 1, {encode(X)});
encode(Term) when is_integer(Term) -> aeb_fate_data:make_integer(Term);
encode(Term) when is_boolean(Term) -> aeb_fate_data:make_boolean(Term);
encode(Term) when is_list(Term) -> aeb_fate_data:make_list([encode(E) || E <- Term]);
encode(Term) when is_tuple(Term) ->
    aeb_fate_data:make_tuple(list_to_tuple([encode(E) || E <- erlang:tuple_to_list(Term)]));
encode(Term) when is_map(Term) ->
    aeb_fate_data:make_map(maps:from_list([{encode(K), encode(V)} || {K,V} <- maps:to_list(Term)]));
encode(Term) when is_binary(Term) -> aeb_fate_data:make_string(Term).

encode_address(Type, S) when is_list(S) ->
    B = list_to_binary(S),
    try aeser_api_encoder:decode(B) of
        {Type, Encoding} ->
            Encoding;
        _ -> erlang:error({bad_address_encoding, Type, S})
    catch _:_ ->
            erlang:error({bad_address_encoding, Type, S})
    end.

decode(I) when ?IS_FATE_INTEGER(I)          -> I;
decode(?FATE_TRUE)                          -> true;
decode(?FATE_FALSE)                         -> false;
decode(L) when ?IS_FATE_LIST(L)             -> [decode(E) || E <- L];
decode(?FATE_ADDRESS(<<Address:256>>))      -> {address, Address};
decode(?FATE_BYTES(B))                      -> {bytes, B};
decode(?FATE_CONTRACT(<<X:256>>))           -> {contract, X};
decode(?FATE_ORACLE(<<X:256>>))             -> {oracle, X};
decode(?FATE_CHANNEL(<<X:256>>))            -> {channel, X};
decode(?FATE_BITS(Bits))                    -> {bits, Bits};
decode(?FATE_TUPLE(T))                      -> list_to_tuple([decode(E) || E <- tuple_to_list(T)]);
decode(?FATE_VARIANT(Arities, Tag, Values)) -> {variant, Arities, Tag, decode(?FATE_TUPLE(Values))};
decode(S) when ?IS_FATE_STRING(S)           -> S;
decode(M) when ?IS_FATE_MAP(M)              ->
    maps:from_list([{decode(K), decode(V)} || {K, V} <- maps:to_list(M)]).

decode(I, word) when ?IS_FATE_INTEGER(I)     -> I;
decode(I, signed_word) when ?IS_FATE_INTEGER(I) -> I;
decode(?FATE_TRUE, bool)                     -> true;
decode(?FATE_FALSE, bool)                    -> false;
decode(L, {list, T}) when ?IS_FATE_LIST(L)   -> [decode(E, T) || E <- L];
decode(?FATE_ADDRESS(<<Address:256>>), word) -> {address, Address};
decode(?FATE_BYTES(B), word)                 -> {bytes, B};
decode(?FATE_CONTRACT(<<X:256>>), word)      -> {contract, X};
decode(?FATE_ORACLE(<<X:256>>), word)        -> {oracle, X};
decode(?FATE_ORACLE_Q(<<X:256>>), word)      -> {oracle_query, X};
decode(?FATE_CHANNEL(<<X:256>>), word)       -> {channel, X};
decode(?FATE_BITS(Bits), word)               -> {bits, Bits};
decode(?FATE_TUPLE({}), word)                -> {tuple, []};
decode(?FATE_TUPLE(Tuple), {tuple, Ts})      ->
    list_to_tuple([decode(E, T) || {E, T} <- lists:zip(tuple_to_list(Tuple), Ts)]);
decode(?FATE_VARIANT(Arities, Tag, Values), {option, Type}) ->
    decode_variant(Arities, [{none, []}, {some, [Type]}], Tag, Values, '$undefined$');
decode(?FATE_VARIANT(Arities, Tag, Values), {variant_t, Cs}) ->
    decode_variant(Arities, Cs, Tag, Values, '$undefined$');
decode(S, string) when ?IS_FATE_STRING(S)           -> S;
decode(M, {map, KeyType, ValType}) when ?IS_FATE_MAP(M)              ->
    maps:from_list([{decode(K, KeyType), decode(V, ValType)} || {K, V} <- maps:to_list(M)]);
decode(T, {tuple, [Single]}) ->
    decode(T, Single).

decode_variant([Arity|Left1], [{C, Types}|Left2], N = 0, Values, Acc) ->
    %% These are the values that should be returned.
    '$undefined$' = Acc,
    case Types of
        [] when Arity =:= 0 ->
            decode_variant(Left1, Left2, N - 1, Values, C);
        [] when Arity =/= 0 ->
            error(variant_type_error);
        [_|_] when length(Types) =:= Arity ->
            Tuple = decode(?FATE_TUPLE(Values), {tuple, Types}),
            Acc1 = list_to_tuple([C | tuple_to_list(Tuple)]),
            decode_variant(Left1, Left2, N - 1, Values, Acc1);
        _ ->
            error({decode_variant_fail, Types, Arity})
    end;
decode_variant([Arity|Left1], [{_C, Types}|Left2], N, Values, Acc) ->
    case length(Types) =:= Arity of
        true ->
            decode_variant(Left1, Left2, N - 1, Values, Acc);
        false ->
            error({decode_variant_fail, Types, Arity})
    end;
decode_variant([], [], _N, _Values, '$undefined$') ->
    error(decode_variant_fail);
decode_variant([], [], _N, _Values, Acc) ->
    Acc.


