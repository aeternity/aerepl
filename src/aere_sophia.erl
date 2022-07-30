-module(aere_sophia).

-export([ typecheck/2, typecheck/1, parse_file/2, parse_file/3, compile_contract/1
        , parse_body/1, parse_top/2
        , parse_decl/1, parse_top/1, parse_type/1, type_of_user_input/1
        , process_err/1, format_value/4
        ]).

-include("../_build/default/lib/aesophia/src/aeso_parse_lib.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").

-include("aere_macros.hrl").

process_err(Errs) when is_list(Errs) ->
    throw({error, lists:concat([aeso_errors:err_msg(E) || E <- Errs])});
process_err(E) -> %% idk, rethrow
    throw(E).


typecheck(Ast) ->
    typecheck(Ast, []).
typecheck(Ast, Opts) ->
    try aeso_ast_infer_types:infer(Ast, [debug_mode, return_env | Opts]) of
        {TEnv, _TAstFolded, TAstUnfolded, _Warns} ->
            {TEnv, TAstUnfolded}
    catch _:{error, Errs} ->
              throw({error, process_err(Errs)})
    end.

compile_contract(TypedAst) ->
    {#{child_con_env := ChildConEnv}, FCode}
        = try aeso_ast_to_fcode:ast_to_fcode(TypedAst, [])
          catch {error, Ec} -> process_err(Ec) end,
    try aeso_fcode_to_fate:compile(ChildConEnv, FCode, [])
    catch {error, Ef} -> process_err(Ef) end.

type_of_user_input(TEnv) ->
    {_, {_, {type_sig, _, _, _, _, Type}}} = aeso_ast_infer_types:lookup_env1(TEnv, term, [], [?MOCK_CONTRACT, ?USER_INPUT]),
    Type.

-define(with_error_handle(X), try X catch {error, Errs} -> process_err(Errs) end).
-spec parse_top(string()) -> [aeso_syntax:decl()] | {body, [aeso_syntax:stmt()]} | none().
parse_top(I) ->
    parse_top(I, []).
-spec parse_top(string(), [term()]) -> [aeso_syntax:decl()] | {body, [aeso_syntax:stmt()]} | none().
parse_top(I, Opts) ->
    Top = aeso_parse_lib:choice
            ([ ?LET_P(Decl, aeso_parser:maybe_block(aeso_parser:decl()),
                      case Decl of
                          _ when is_list(Decl) -> Decl;
                          _ -> [Decl]
                      end),
               ?LET_P(Body, aeso_parser:body(),
                      case Body of
                          {block, _, Stmts} when is_list(Stmts) ->
                              ?IF(lists:all(fun(X) -> element(1, X) =:= letval end, Stmts),
                                  aeso_parse_lib:fail(),
                                  {body, Stmts}
                                 );
                          LV when element(1, LV) =:= letval -> aeso_parse_lib:fail();
                          _ -> {body, [Body]}
                      end)
             ]),
    ?with_error_handle(aeso_parser:run_parser(Top, I, Opts)).
parse_decl(I) ->
    ?with_error_handle(aeso_parser:run_parser(aeso_parser:decl(), I)).
parse_body(I) ->
    ?with_error_handle(
       case aeso_parser:run_parser(aeso_parser:body(), I) of
           {block, _, Stmts} when is_list(Stmts) -> Stmts;
           Other -> [Other]
       end).
parse_type(I) ->
    ?with_error_handle(aeso_parser:run_parser(aeso_parser:type(), I)).
parse_file(I, Opts) ->
    parse_file(I, sets:new(), Opts).
parse_file(I, Includes, Opts) when is_binary(I) ->
    parse_file(binary:bin_to_list(I), Includes, Opts);
parse_file(I, Includes, Opts) ->
    ?with_error_handle(aeso_parser:string(I, Includes, Opts)).

format_value(sophia, TEnv, Type, Val) ->
    Sophia = fate_to_sophia(#{}, TEnv, Type, Val),
    prettypr:format(aeso_pretty:expr(Sophia));
format_value(json, TEnv, Type, Val) ->
    Sophia = fate_to_sophia(#{}, TEnv, Type, Val),
    JSON = jsx:encode(aeso_aci:json_encode_expr(drop_typed(Sophia))),
    binary:bin_to_list(jsx:prettify(JSON)).


-spec fate_to_sophia(#{string() => aeso_syntax:type()}, term(), aeso_syntax:type() | aeso_syntax:typedef(), term()) -> aeso_syntax:expr().
fate_to_sophia(Subst, TEnv, {tvar, _, TV}, Val) ->
    fate_to_sophia(Subst, TEnv, maps:get(TV, Subst), Val);
fate_to_sophia(_, _, _, I) when ?IS_FATE_INTEGER(I) ->
    {int, [], I};
fate_to_sophia(_, _, _, ?FATE_TRUE) ->
    {bool, [], true};
fate_to_sophia(_, _, _, ?FATE_FALSE) ->
    {bool, [], false};
fate_to_sophia(Subst, TEnv, {app_t, _, {id, _, "list"}, [ElT]}, L) when ?IS_FATE_LIST(L) ->
    Lp = [fate_to_sophia(Subst, TEnv, ElT, E) || E <- L],
    {list, [], Lp};
fate_to_sophia(Subst, TEnv, {app_t, _, {id, _, "map"}, [KeyT, ValT]}, M) when ?IS_FATE_MAP(M) ->
    Mp =
        [ {fate_to_sophia(Subst, TEnv, KeyT, K), fate_to_sophia(Subst, TEnv, ValT, V)}
          || {K, V} <- maps:to_list(M)
        ],
    {map, [], Mp};
fate_to_sophia(Subst, TEnv, {app_t, _, Id, Args}, Val) ->
    Name = aeso_ast_infer_types:qname(Id),
    E = {_, {_, {TArgs, _}}} = aeso_ast_infer_types:lookup_env1(TEnv, type, [], Name),
    io:format("AAAA ~p\n\n", [E]),
    Inst = lists:zip([T || {tvar, _, T} <- TArgs], Args),
    Subst1 = maps:merge(maps:from_list(Inst), Subst),
    fate_to_sophia(Subst1, TEnv, Id, Val);
fate_to_sophia(_, _, _, ?FATE_ADDRESS(Addr)) ->
    {account_pubkey, [], Addr};
fate_to_sophia(_, _, _, ?FATE_CONTRACT(Addr)) ->
    {contract_pubkey, [], Addr};
fate_to_sophia(_, _, _, ?FATE_ORACLE(Addr)) ->
    {oracle_pubkey, [], Addr};
fate_to_sophia(_, _, _, ?FATE_ORACLE_Q(Addr)) ->
    {oracle_query_id, [], Addr};
fate_to_sophia(_, _, _, ?FATE_BYTES(Bs)) ->
    {bytes, [], Bs};
fate_to_sophia(_, _, _, ?FATE_BITS(Bits)) ->
    {bytes, [], Bits}; %% Sophia doesn't have bits literal, so let's print it as bytes
fate_to_sophia(Subst, TEnv, {tuple_t, _, Ts}, ?FATE_TUPLE(Tuple)) ->
    Elems = lists:zip(Ts, tuple_to_list(Tuple)),
    ElemsP = [fate_to_sophia(Subst, TEnv, T, E) || {T, E} <- Elems],
    {tuple, [], ElemsP};
fate_to_sophia(Subst, TEnv, {record_t, FieldsT}, ?FATE_TUPLE(Tuple)) ->
    Elems = lists:zip(FieldsT, tuple_to_list(Tuple)),
    ElemsP = [{field, [], [{proj, [], FId}], fate_to_sophia(Subst, TEnv, FT, E)}
              || {{field_t, _, FId, FT}, E} <- Elems],
    {record, [], ElemsP};
fate_to_sophia(_Subst, _TEnv, Type = {fun_t, _, _, _, _}, ?FATE_TUPLE({FName, _Closure})) -> % closure
    HexName = binary:encode_hex(FName),
    Display = <<"<fun ", HexName/binary, ">">>,
    {typed, [], {string, [], Display}, Type};
fate_to_sophia(Subst, TEnv, Id, Val = ?FATE_TUPLE(_)) -> % Record ID
    Name = aeso_ast_infer_types:qname(Id),
    {_, {_, {_, Record = {record_t, _}}}} = aeso_ast_infer_types:lookup_env1(TEnv, type, [], Name),
    fate_to_sophia(Subst, TEnv, Record, Val);
fate_to_sophia(Subst, TEnv, Id, ?FATE_VARIANT(_Arities, Tag, Values)) ->
    Name = aeso_ast_infer_types:qname(Id),
    {_, {_, {_, {variant_t, Constrs}}}} = aeso_ast_infer_types:lookup_env1(TEnv, type, [], Name),
    {constr_t, _, Con, TArgs} = lists:nth(Tag + 1, Constrs),
    PValues = [fate_to_sophia(Subst, TEnv, T, V) || {T, V} <- lists:zip(TArgs, tuple_to_list(Values))],
    {app, [], Con, PValues};
fate_to_sophia(_, _, _, S) when ?IS_FATE_STRING(S) ->
    {string, [], S}.

drop_typed({typed, _, Expr, T}) ->
    drop_typed(Expr);
drop_typed(L) when is_list(L) ->
    lists:map(fun drop_typed/1, L);
drop_typed(T) when is_tuple(T) ->
    list_to_tuple(drop_typed(tuple_to_list(T)));
drop_typed(M) when is_map(M) ->
    maps:from_list(drop_typed(maps:to_list(M)));
drop_typed(Expr) ->
    Expr.
