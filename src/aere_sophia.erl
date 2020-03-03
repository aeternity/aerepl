-module(aere_sophia).

-export([ typecheck/2, typecheck/1, parse_file/2, parse_file/3, compile_contract/3
        , parse_body/1, parse_decl/1, parse_top/1, parse_type/1, type_of/2
        , generate_interface_decl/1, process_err/1, get_pat_ids/1
        , replace_ast/4
        ]).

-include("../_build/default/lib/aesophia/src/aeso_parse_lib.hrl").
-include("aere_repl.hrl").

process_err(Errs) when is_list(Errs) ->
    throw({error, lists:concat([aeso_errors:err_msg(E) || E <- Errs])});
process_err(E) -> %% idk, rethrow
    throw(E).


typecheck(Ast) ->
    typecheck(Ast, []).
typecheck(Ast, Opts) ->
    try aeso_ast_infer_types:infer(Ast, Opts)
    catch _:{error, Errs} ->
            throw({error, process_err(Errs)})
    end.


compile_contract(fate, Src, TypedAst) ->
    FCode    = try aeso_ast_to_fcode:ast_to_fcode(TypedAst, [])
               catch {error, Ec} -> process_err(Ec) end,
    Fate     = try aeso_fcode_to_fate:compile(FCode, [])
               catch {error, Ef} -> process_err(Ef) end,
    ByteCode = aeb_fate_code:serialize(Fate, []),
    #{byte_code => ByteCode,
      contract_source => Src,
      type_info => [],
      fate_code => Fate,
      compiler_version => aere_version:sophia_version(fate),
      abi_version => aere_version:abi_version(fate),
      payable => maps:get(payable, FCode)
     };
compile_contract(aevm, Src, TypedAst) ->
    Icode = try aeso_ast_to_icode:convert_typed(TypedAst, [])
            catch {error, Ei} -> process_err(Ei) end,
    TypeInfo  = extract_type_info(Icode),
    Assembler = aeso_icode_to_asm:convert(Icode, []),
    ByteCodeList = to_bytecode(Assembler, []),
    ByteCode = << << B:8 >> || B <- ByteCodeList >>,
    #{byte_code => ByteCode,
      contract_source => Src,
      type_info => TypeInfo,
      compiler_version => aere_version:sophia_version(aevm),
      abi_version => aeb_aevm_abi:abi_version(),
      payable => maps:get(payable, Icode)
     }.

type_of([{contract, _, _, Defs}], FunName) ->
    ArgType = fun(A) -> [T || {arg, _, _, T} <- A] end,
    GetType = fun({letfun, _, {id, _, Name}, Args, Ret, _})
                    when Name == FunName -> [{Args, Ret}];
                 ({fun_decl, _, {id, _, Name}, {fun_t, _, _, Args, Ret}})
                    when Name == FunName -> [{Args, Ret}];
                 (_) -> [] end,
    case lists:flatmap(GetType, Defs) of
        [{Args, Ret}] -> {ArgType(Args), Ret};
        []            ->
            case FunName of
                "init" -> {[], {tuple_t, [], []}};
                _ -> error("Function " ++ FunName ++ " is not defined")
            end;
        _ -> error("what the fuck, two entrypoints with the same name")
    end;
type_of([_ | Contracts], FunName) ->
    type_of(Contracts, FunName).


extract_type_info(#{functions := Functions} =_Icode) ->
    ArgTypesOnly = fun(As) -> [ T || {_, T} <- As ] end,
    Payable = fun(Attrs) -> proplists:get_value(payable, Attrs, false) end,
    TypeInfo = [aeb_aevm_abi:function_type_info(list_to_binary(lists:last(Name)),
                                                Payable(Attrs), ArgTypesOnly(Args), TypeRep)
                || {Name, Attrs, Args,_Body, TypeRep} <- Functions,
                   not is_tuple(Name),
                   not lists:member(private, Attrs)
               ],
    lists:sort(TypeInfo).


to_bytecode(['COMMENT',_|Rest],_Options) ->
    to_bytecode(Rest,_Options);
to_bytecode([Op|Rest], Options) ->
    [aeb_opcodes:m_to_op(Op)|to_bytecode(Rest, Options)];
to_bytecode([], _) -> [].


-define(with_error_handle(X), try X catch {error, Errs} -> process_err(Errs) end).
parse_top(I) ->
    Top = aeso_parse_lib:choice
            ([ aeso_parser:maybe_block(aeso_parser:decl()),
               ?LET_P(Stmts, aeso_parser:maybe_block(aeso_parser:stmt()),
                      case lists:all(fun(X) -> element(1, X) =:= letval end, Stmts) of
                          true -> aeso_parse_lib:fail();
                          false -> {body, Stmts}
                      end)
             ]),
    ?with_error_handle(aeso_parser:run_parser(Top, I)).
parse_decl(I) ->
    ?with_error_handle(aeso_parser:run_parser(aeso_parser:decl(), I)).
parse_body(I) ->
    ?with_error_handle(aeso_parser:run_parser(aeso_parser:maybe_block(aeso_parser:stmt()), I)).
parse_type(I) ->
    ?with_error_handle(aeso_parser:run_parser(aeso_parser:type(), I)).
parse_file(I, Opts) ->
    parse_file(I, sets:new(), Opts).
parse_file(I, Includes, Opts) ->
    ?with_error_handle(aeso_parser:string(I, Includes, Opts)).


generate_interface_decl([{contract, Ann, Name, Funs}]) ->
    {Name, {contract, Ann, Name, get_funs_decls(Funs)}};
generate_interface_decl([_|Rest]) ->
    generate_interface_decl(Rest);
generate_interface_decl([]) ->
    error("Empty contract?").


get_funs_decls(Funs) ->
    get_funs_decls(Funs, []).
get_funs_decls([], Acc) ->
    Acc;
get_funs_decls([{letfun, Ann, Name, Args, RetType, _}|Rest], Acc) ->
    TArgs = [T || {typed, _, _, T} <- Args],
    get_funs_decls(Rest, [{fun_decl, Ann, Name, {fun_t, Ann, [], TArgs, RetType}}|Acc]);
get_funs_decls([{fun_clauses, Ann, Name, Type, _Bodies}|Rest], Acc) ->
    get_funs_decls(Rest, [{fun_decl, Ann, Name, Type}|Acc]);
get_funs_decls([Decl | Rest], Acc) when element(1, Decl) =:= fun_decl
                                        orelse element(1, Decl) =:= type_decl
                                        orelse element(1, Decl) =:= type_def
                                        ->
    get_funs_decls(Rest, [Decl | Acc]).



get_pat_ids({app, _, _, Pts}) ->
    [I || P <- Pts, I <- get_pat_ids(P)];
get_pat_ids({tuple, _, Pts}) ->
    [I || P <- Pts, I <- get_pat_ids(P)];
get_pat_ids({list, _, Pts}) ->
    [I || P <- Pts, I <- get_pat_ids(P)];
get_pat_ids({typed, _, P, _}) ->
    get_pat_ids(P);
get_pat_ids({record, _, Flds}) ->
    [I || {field, _, _, P} <- Flds, I <- get_pat_ids(P)] ++
        [I || {field, _, _, _, P} <- Flds, I <- get_pat_ids(P)];
get_pat_ids({id, _, I}) ->
    [I];
get_pat_ids(_) ->
    [].


replace_ast(B, Pat, Old, New) when is_list(B) ->
    replace_ast(B, Pat, Old, New, []);
replace_ast(E, Pat, Old, New) ->
    replace_ast1(E, Pat, Old, New).
replace_ast([], _, _, _, Acc) ->
    lists:reverse(Acc);
replace_ast([{letval, Ann, Pats, Expr}|Rest], Pat, Old, New, Acc) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    Stmt = {letval, Ann, Pats, replace_ast1(Expr, Pat, Old, New)},
    case lists:member(Old, PatIds) of
        true -> lists:reverse([Stmt|Acc]) ++ Rest;
        false -> replace_ast(Rest, Pat, Old, New, [Stmt|Acc])
    end;
replace_ast([{letfun, Ann, Name, Pats, RT, Expr}|Rest], Pat, Old, New, Acc) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    Stmt = {letfun, Ann, Name, Pats, RT, replace_ast1(Expr, Pat, Old, New)},
    case lists:member(Old, PatIds) of
        true -> lists:reverse([Stmt|Acc]) ++ Rest;
        false -> replace_ast(Rest, Pat, Old, New, [Stmt|Acc])
    end;
replace_ast([{fun_decl, Ann, Name, RT}|Rest], Pat, Old, New, Acc) ->
    replace_ast(Rest, Pat, Old, New, [{fun_decl, Ann, Name, replace_ast1(RT, Pat, Old, New)}|Acc]);
replace_ast([Expr|Rest], Pat, Old, New, Acc) ->
    replace_ast(Rest, Pat, Old, New, [replace_ast1(Expr, Pat, Old, New)|Acc]).

replace_ast1({id, A, Old}, id, Old, New) when is_list(New) ->
    {id, A, New};
replace_ast1({id, _A, Old}, id, Old, New) ->
    New;
replace_ast1({qid, _A, [Old]}, id, Old, New) ->
    New;
replace_ast1({qid, _A, Old}, id, Old, New) ->
    New;
replace_ast1({con, _A, Old}, id, Old, New) ->
    New;
replace_ast1({qcon, _A, [Old]}, id, Old, New) ->
    New;
replace_ast1({qcon, _A, Old}, id, Old, New) ->
    New;
replace_ast1({lam, Ann, Args, Expr}, Pat, Old, New) ->
    ArgIds = [I || {arg, _, I, _} <- Args],
    Expr1 = case lists:member(Old, ArgIds) of
                true -> Expr;
                false -> replace_ast(Expr, Pat, Old, New)
            end,
    {lam, Ann, Args, Expr1};
replace_ast1({'if', Ann, C, T, E}, Pat, Old, New) ->
    {'if', Ann, replace_ast1(C, Pat, Old, New),
     replace_ast1(T, Pat, Old, New), replace_ast1(E, Pat, Old, New)};
replace_ast1({switch, Ann, Expr, Alts}, Pat, Old, New) ->
    {switch, Ann, replace_ast1(Expr, Pat, Old, New),
     [ begin
           Expr1 = case lists:member(Old, get_pat_ids(P)) of
                       true -> ExprA;
                       false -> replace_ast(ExprA, Pat, Old, New)
                   end,
           {'case', A, P, Expr1}
       end
      || {'case', A, P, ExprA} <- Alts
     ]};
replace_ast1({app, Ann, Expr, Args}, Pat, Old, New) ->
    {app, Ann, replace_ast1(Expr, Pat, Old, New), [replace_ast1(A, Pat, Old, New) || A <- Args]};
replace_ast1({proj, Ann, Expr, Id}, Pat, Old, New) ->
    {proj, Ann, replace_ast1(Expr, Pat, Old, New), Id};
replace_ast1({tuple, Ann, Exprs}, Pat, Old, New) ->
    {tuple, Ann, [replace_ast1(Expr, Pat, Old, New) || Expr <- Exprs]};
replace_ast1({list, Ann, Exprs}, Pat, Old, New) ->
    {list, Ann, [replace_ast1(Expr, Pat, Old, New) || Expr <- Exprs]};
replace_ast1({list_comp, Ann, Expr, [{comprehension_bind, Pat, BindBy}|Rest]}, Pat, Old, New) ->
    BindBy1 = replace_ast1(BindBy, Pat, Old, New),
    {list_comp, _, Expr1, Rest1} =
        case lists:member(Old, get_pat_ids(Pat)) of
            true -> {list_comp, Ann, Expr, Rest};
            false -> replace_ast({list_comp, Ann, Expr, Rest}, Pat, Old, New)
        end,
    {list_comp, Ann, Expr1, [{comprehension_bind, Pat, BindBy1}|Rest1]};
replace_ast1({list_comp, Ann, Expr, [{comprehension_if, A, Cond}|Rest]}, Pat, Old, New) ->
    Cond1 = replace_ast1(Cond, Pat, Old, New),
    {list_comp, _, Expr1, Rest1} =
            replace_ast({list_comp, Ann, Expr, Rest}, Pat, Old, New),
    {list_comp, Ann, Expr1, [{comprehension_if, A, Cond1}|Rest1]};
replace_ast1({list_comp, Ann, Expr, [{letfun, Ann, Name, Pats, RT, LExpr}|Rest]}, Pat, Old, New) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    LExpr1 = replace_ast(LExpr, Pat, Old, New),
    {list_comp, _, Expr1, Rest1} =
        case lists:member(Old, PatIds) of
            true -> {list_comp, Ann, Expr, Rest};
            false -> replace_ast({list_comp, Ann, Expr, Rest}, Pat, Old, New)
        end,
    Let1 = {letfun, Ann, Name, Pats, RT, LExpr1},
    {list_comp, Ann, Expr1, [Let1|Rest1]};
replace_ast1({list_comp, Ann, Expr, [{letval, Ann, Pats, LExpr}|Rest]}, Pat, Old, New) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    LExpr1 = replace_ast(LExpr, Pat, Old, New),
    {list_comp, _, Expr1, Rest1} =
        case lists:member(Old, PatIds) of
            true -> {list_comp, Ann, Expr, Rest};
            false -> replace_ast({list_comp, Ann, Expr, Rest}, Pat, Old, New)
        end,
    Let1 = {letval, Ann, Pats, LExpr1},
    {list_comp, Ann, Expr1, [Let1|Rest1]};
replace_ast1({list_comp, Ann, Expr, []}, Pat, Old, New) ->
    {list_comp, Ann, replace_ast1(Expr, Pat, Old, New), []};
replace_ast1({typed, Ann, Expr, T}, Pat, Old, New) ->
    {typed, Ann, replace_ast1(Expr, Pat, Old, New), replace_ast1(T, Pat, Old, New)};
replace_ast1({record, Ann, Fields}, Pat, Old, New) ->
    {record, Ann,
    [ case F of
          {field, A, LV, E} ->
              {field, A, LV, replace_ast1(E, Pat, Old, New)};
          {field, A, LV, I, E} ->
              {field, A, LV, I, replace_ast1(E, Pat, Old, New)}
      end
      || F <- Fields
    ]};
replace_ast1({record, Ann, Expr, Fields}, Pat, Old, New) ->
    {record, Ann, replace_ast1(Expr, Pat, Old, New),
     [ case F of
           {field, A, LV, E} ->
               {field, A, LV, replace_ast1(E, Pat, Old, New)};
           {field, A, LV, I, E} ->
               {field, A, LV, I, replace_ast1(E, Pat, Old, New)}
       end
       || F <- Fields
     ]};
replace_ast1({map, Ann, Fields}, Pat, Old, New) ->
    {map, Ann,
     [ case F of
           {field, A, LV, E} ->
               {field, A, LV, replace_ast1(E, Pat, Old, New)};
           {field, A, LV, I, E} ->
               {field, A, LV, I, replace_ast1(E, Pat, Old, New)}
       end
       || F <- Fields
     ]};
replace_ast1({map, Ann, Expr, Fields}, Pat, Old, New) ->
    {map, Ann, replace_ast1(Expr, Pat, Old, New),
     [ case F of
           {field, A, LV, E} ->
               {field, A, LV, replace_ast1(E, Pat, Old, New)};
           {field, A, LV, I, E} ->
               {field, A, LV, I, replace_ast1(E, Pat, Old, New)}
       end
       || F <- Fields
     ]};
replace_ast1({map, Ann, Pairs}, Pat, Old, New) ->
    {map, Ann, [{replace_ast1(K, Pat, Old, New), replace_ast1(V, Pat, Old, New)}
                || {K, V} <- Pairs]};
replace_ast1({map_get, Ann, M, K}, Pat, Old, New) ->
    {map_get, Ann, replace_ast1(M, Pat, Old, New), replace_ast1(K, Pat, Old, New)};
replace_ast1({map_get, Ann, M, K, D}, Pat, Old, New) ->
    {map_get, Ann, replace_ast1(M, Pat, Old, New), replace_ast1(K, Pat, Old, New),
     replace_ast1(D, Pat, Old, New)};
replace_ast1({block, Ann, Stmts}, Pat, Old, New) ->
    {block, Ann, replace_ast(Stmts, Pat, Old, New)};
replace_ast1(Other, _, _, _) ->
    Other.
