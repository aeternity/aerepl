-module(aere_sophia).

-export([ compile_contract/1
        , typecheck/2
        , typecheck/1
        , type_of_user_input/1
        , parse_file/2
        , parse_file/3
        , parse_body/1
        , parse_top/1
        , format_value/4
        ]).

-include("../_build/default/lib/aesophia/src/aeso_parse_lib.hrl").
-include_lib("aebytecode/include/aeb_fate_data.hrl").

-include("aere_macros.hrl").

-type parse_result() :: [aeso_syntax:decl()] | {body, [aeso_syntax:stmt()]}.

-export_type([parse_result/0]).

optimizations_off() ->
    [ {optimize_inliner, false},
      {optimize_inline_local_functions, false},
      {optimize_bind_subexpressions, false},
      {optimize_let_floating, false},
      {optimize_simplifier, false},
      {optimize_drop_unused_lets, false},
      {optimize_push_consume, false},
      {optimize_one_shot_var, false},
      {optimize_write_to_dead_var, false},
      {optimize_inline_switch_target, false},
      {optimize_swap_push, false},
      {optimize_swap_pop, false},
      {optimize_swap_write, false},
      {optimize_constant_propagation, false},
      {optimize_prune_impossible_branches, false},
      {optimize_single_successful_branch, false},
      {optimize_inline_store, false},
      {optimize_float_switch_bod, false} ].

process_err(Errs) when is_list(Errs) ->
    throw({repl_error, aere_msg:error(lists:concat([aeso_errors:err_msg(E) || E <- Errs]))});
process_err(E) -> %% idk, rethrow
    throw(E).


typecheck(Ast) ->
    typecheck(Ast, []).
typecheck(Ast, Opts) ->
    try aeso_ast_infer_types:infer(Ast, [debug_mode, return_env | Opts]) of
        {TEnv, _TAstFolded, TAstUnfolded, _Warns} ->
            {TEnv, TAstUnfolded}
    catch _:{error, Errs} ->
              throw({repl_error, process_err(Errs)})
    end.

compile_contract(TypedAst) ->
    Opts = [debug_info, include_child_contract_symbols | optimizations_off()],
    {#{child_con_env := ChildConEnv, saved_fresh_names := SavedFreshNames}, FCode}
        = try aeso_ast_to_fcode:ast_to_fcode(TypedAst, Opts)
          catch {error, Ec} -> process_err(Ec) end,
    try
        aeso_fcode_to_fate:compile(ChildConEnv, FCode, SavedFreshNames, Opts)
    catch {error, Ef} -> process_err(Ef) end.

type_of_user_input(TEnv0) ->
    %% TODO: This is a hack, not a solution. If not done this way, the error
    %% contract_treated_as_namespace_entrypoint will show when calling lookup_env1
    TEnv = setelement(7, TEnv0, [?MOCK_CONTRACT]),

    {_, {_, {type_sig, _, _, _, _, Type}}} = aeso_ast_infer_types:lookup_env1(TEnv, term, [], [?MOCK_CONTRACT, ?USER_INPUT]),
    Type.

-define(with_error_handle(X), try X catch {error, Errs} -> process_err(Errs) end).
-spec parse_top(string()) -> parse_result() | none().
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

parse_body(I) ->
    ?with_error_handle(
       case aeso_parser:run_parser(aeso_parser:body(), I) of
           {block, _, Stmts} when is_list(Stmts) -> Stmts;
           Other -> [Other]
       end).
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
fate_to_sophia(_, _, {app_t, _, {id, _, "option"}, [_]}, ?FATE_VARIANT(_, 0, {})) ->
    {con, [], "None"};
fate_to_sophia(Subst, TEnv, {app_t, _, {id, _, "option"}, [ElT]}, ?FATE_VARIANT(_, 1, {Val})) ->
    {app, [], {con, [], "Some"}, [fate_to_sophia(Subst, TEnv, ElT, Val)]};
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
    {_, {_, {TArgs, _}}} = aeso_ast_infer_types:lookup_env1(TEnv, type, [], Name),
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
fate_to_sophia(Subst, TEnv, Id, Expr = ?FATE_VARIANT(_Arities, Tag, Values)) ->
    Name = aeso_ast_infer_types:qname(Id),
    {_, {_, Def}} = aeso_ast_infer_types:lookup_env1(TEnv, type, [], Name),
    case Def of
        {_, {variant_t, Constrs}} ->
            {constr_t, _, Con, TArgs} = lists:nth(Tag + 1, Constrs),
            PValues = [fate_to_sophia(Subst, TEnv, T, V) || {T, V} <- lists:zip(TArgs, tuple_to_list(Values))],
            {app, [], Con, PValues};
        {builtin, 0} ->
            % Since arity is 0, there is no need to expand the
            % type args with respect to TEnv and Subst
            aeso_vm_decode:from_fate(Id, Expr)
    end;
fate_to_sophia(_, _, _, S) when ?IS_FATE_STRING(S) ->
    {string, [], S}.

drop_typed({typed, _, Expr, _}) ->
    drop_typed(Expr);
drop_typed(L) when is_list(L) ->
    lists:map(fun drop_typed/1, L);
drop_typed(T) when is_tuple(T) ->
    list_to_tuple(drop_typed(tuple_to_list(T)));
drop_typed(M) when is_map(M) ->
    maps:from_list(drop_typed(maps:to_list(M)));
drop_typed(Expr) ->
    Expr.
