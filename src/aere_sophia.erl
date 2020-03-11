-module(aere_sophia).

-export([ typecheck/2, typecheck/1, parse_file/2, parse_file/3, compile_contract/3
        , parse_body/1, parse_top/2
        , parse_decl/1, parse_top/1, parse_type/1, type_of/2
        , generate_interface_decl/1, process_err/1, get_pat_ids/1
        , replace_ast/4, replace_type_in_decls/4, replace_type/4        ]).

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
        _ -> error("co kurwa, two entrypoints with the same name")
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
    parse_top(I, []).
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


%% This is real shit. Legend says that this function reduces
%% to the halting problem.
-define(ReplaceAST(X), replace_ast(X, Pat, Old, New)).
-define(ReplaceType(T), replace_type(T, Pat, Old, New)).
-define(ReplaceBlock(X), replace_ast_block(X, Pat, Old, New)).
-define(ReplaceTypeInExpr(T), case Pat =:= type_id of
                                  true -> ?ReplaceType(T);
                                  false -> T
                              end).
replace_ast({id, A, Old}, id, Old, New) when is_list(New) ->
    {id, A, New};
replace_ast({qid, A, QOld}, id, Old, New) when is_list(New) ->
    {qid, A, lists:map(fun(N) -> ?IF(N == Old, New, N) end, QOld)};
replace_ast({id, _A, Old}, id, Old, New) ->
    New;
replace_ast({qid, _A, [Old]}, id, Old, New) ->
    New;
replace_ast({qid, _A, Old}, id, Old, New) ->
    New;
replace_ast({con, _A, Old}, id, Old, New) ->
    New;

replace_ast(L, Pat, Old, New) when is_list(L) ->
    ?ReplaceBlock(L);
replace_ast({lam, Ann, Args, Expr}, Pat, Old, New) ->
    ArgIds = [I || {arg, _, I, _} <- Args],
    Expr1 = ?IF(lists:member(Old, ArgIds), Expr, ?ReplaceBlock(Expr)),
    {lam, Ann, Args, Expr1};
replace_ast({'if', Ann, C, T, E}, Pat, Old, New) ->
    {'if', Ann, ?ReplaceAST(C), ?ReplaceAST(T), ?ReplaceAST(E)};
replace_ast({switch, Ann, Expr, Alts}, Pat, Old, New) ->
    {switch, Ann, ?ReplaceAST(Expr),
     [ begin
           Expr1 = ?IF(lists:member(Old, get_pat_ids(P)), ExprA, ?ReplaceBlock(ExprA)),
           {'case', A, P, Expr1}
       end
      || {'case', A, P, ExprA} <- Alts
     ]};
replace_ast({app, Ann, Expr, Args}, Pat, Old, New) ->
    {app, Ann, ?ReplaceAST(Expr),  [?ReplaceAST(A) || A <- Args]};
replace_ast({proj, Ann, Expr, Id}, Pat, Old, New) ->
    {proj, Ann, ?ReplaceAST(Expr), Id};
replace_ast({tuple, Ann, Exprs}, Pat, Old, New) ->
    {tuple, Ann,  [?ReplaceAST(Expr) || Expr <- Exprs]};
replace_ast({list, Ann, Exprs}, Pat, Old, New) ->
    {list, Ann,  [?ReplaceAST(Expr) || Expr <- Exprs]};
replace_ast({list_comp, Ann, Expr, [{comprehension_bind, Pat, BindBy}|Rest]}, Pat, Old, New) ->
    BindBy1 = ?ReplaceAST(BindBy),
    {list_comp, _, Expr1, Rest1} =
        ?IF(lists:member(Old, get_pat_ids(Pat)),
            {list_comp, Ann, Expr, Rest},
            ?ReplaceAST({list_comp, Ann, Expr, Rest})
           ),
    {list_comp, Ann, Expr1, [{comprehension_bind, Pat, BindBy1}|Rest1]};
replace_ast({list_comp, Ann, Expr, [{comprehension_if, A, Cond}|Rest]}, Pat, Old, New) ->
    Cond1 = ?ReplaceAST(Cond),
    {list_comp, _, Expr1, Rest1} =
            ?ReplaceAST({list_comp, Ann, Expr, Rest}),
    {list_comp, Ann, Expr1, [{comprehension_if, A, Cond1}|Rest1]};
replace_ast({list_comp, Ann, Expr, [{letfun, Ann, Name, Pats, RT, LExpr}|Rest]}, Pat, Old, New) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    LExpr1 = ?ReplaceBlock(LExpr),
    {list_comp, _, Expr1, Rest1} =
        ?IF(lists:member(Old, PatIds),
            {list_comp, Ann, Expr, Rest},
            ?ReplaceAST({list_comp, Ann, Expr, Rest})
           ),
    Let1 = {letfun, Ann, Name, Pats, ?ReplaceTypeInExpr(RT), LExpr1},
    {list_comp, Ann, Expr1, [Let1|Rest1]};
replace_ast({list_comp, Ann, Expr, [{letval, Ann, Pats, LExpr}|Rest]}, Pat, Old, New) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    LExpr1 = ?ReplaceBlock(LExpr),
    {list_comp, _, Expr1, Rest1} =
        ?IF(lists:member(Old, PatIds),
            {list_comp, Ann, Expr, Rest},
            ?ReplaceAST({list_comp, Ann, Expr, Rest})
           ),
    Let1 = {letval, Ann, Pats, LExpr1},
    {list_comp, Ann, Expr1, [Let1|Rest1]};
replace_ast({list_comp, Ann, Expr, []}, Pat, Old, New) ->
    {list_comp, Ann, ?ReplaceAST(Expr), []};
replace_ast({typed, Ann, Expr, T}, Pat, Old, New) ->
    {typed, Ann, ?ReplaceAST(Expr), ?ReplaceTypeInExpr(T)};
replace_ast({record, Ann, Fields}, Pat, Old, New) ->
    {record, Ann,
    [ case F of
          {field, A, LV, E}    -> {field, A, LV, ?ReplaceAST(E)};
          {field, A, LV, I, E} -> {field, A, LV, I, ?ReplaceAST(E)}
      end
      || F <- Fields
    ]};
replace_ast({record, Ann, Expr, Fields}, Pat, Old, New) ->
    {record, Ann, ?ReplaceAST(Expr),
     [ case F of
           {field, A, LV, E} ->
               {field, A, LV, ?ReplaceAST(E)};
           {field, A, LV, I, E} ->
               {field, A, LV, I, ?ReplaceAST(E)}
       end
       || F <- Fields
     ]};
replace_ast({map, Ann, Fields}, Pat, Old, New) ->
    {map, Ann,
     [ case F of
           {field, A, LV, E} ->
               {field, A, LV, ?ReplaceAST(E)};
           {field, A, LV, I, E} ->
               {field, A, LV, I, ?ReplaceAST(E)}
       end
       || F <- Fields
     ]};
replace_ast({map, Ann, Expr, Fields}, Pat, Old, New) ->
    {map, Ann, ?ReplaceAST(Expr),
     [ case F of
           {field, A, LV, E} ->
               {field, A, LV, ?ReplaceAST(E)};
           {field, A, LV, I, E} ->
               {field, A, LV, I, ?ReplaceAST(E)}
       end
       || F <- Fields
     ]};
replace_ast({map, Ann, Pairs}, Pat, Old, New) ->
    {map, Ann, [{replace_ast(K, Pat, Old, New), ?ReplaceAST(V)}
                || {K, V} <- Pairs]};
replace_ast({map_get, Ann, M, K}, Pat, Old, New) ->
    {map_get, Ann, ?ReplaceAST(M), ?ReplaceAST(K)};
replace_ast({map_get, Ann, M, K, D}, Pat, Old, New) ->
    {map_get, Ann, ?ReplaceAST(M), ?ReplaceAST(K),
     ?ReplaceAST(D)};
replace_ast({block, Ann, Stmts}, Pat, Old, New) ->
    {block, Ann, ?ReplaceBlock(Stmts)};
replace_ast(Other, _, _, _) ->
    Other.


%% Types – once we enter type replacement there is no need
%% to use checks from ?ReplaceType
replace_type({id, A, Old}, _, Old, New) when is_list(New) ->
    {id, A, New};
replace_type({qid, A, QOld}, _, Old, New) when is_list(New) ->
    {qid, A, lists:map(fun(N) -> ?IF(N == Old, New, N) end, QOld)};
replace_type({id, _A, Old}, _, Old, New) ->
    New;
replace_type({qid, _A, [Old]}, _, Old, New) ->
    New;
replace_type({qid, _A, Old}, _, Old, New) ->
    New;
replace_type({con, _A, Old}, _, Old, New) ->
    New;
replace_type({qcon, _A, [Old]}, _, Old, New) ->
    New;
replace_type({qcon, _A, Old}, _, Old, New) ->
    New;
replace_type({fun_t, Ann, NArgs, Args, Ret}, Pat, Old, New) ->
    { fun_t, Ann
    , [?ReplaceType(NA) || NA <- NArgs]
    , [?ReplaceType(A) || A <- Args]
    , ?ReplaceType(Ret)};
replace_type({app_t, Ann, T, Args}, Pat, Old, New) ->
    { app_t, Ann, ?ReplaceType(T)
    , [?ReplaceType(A) || A <- Args]};
replace_type({tuple_t, Ann, Args}, Pat, Old, New) ->
    { tuple_t, Ann
    , [?ReplaceType(A) || A <- Args]};
replace_type({args_t, Ann, Args}, Pat, Old, New) ->
    { args_t, Ann
    , [?ReplaceType(A) || A <- Args]};
replace_type({named_arg_t, Ann, Id, T, Expr}, Pat, Old, New) ->
    { named_arg_t, Ann, Id, ?ReplaceType(T)
    , ?ReplaceType(Expr)};
replace_type({alias_t, T}, Pat, Old, New) ->
    {alias_t, ?ReplaceType(T)};
replace_type({record_t, Flds}, Pat, Old, New) ->
    {record_t,  [?ReplaceType(F) || F <- Flds]};
replace_type({variant_t, Constrs}, Pat, Old, New) ->
    {variant_t,  [?ReplaceType(C) || C <- Constrs]};
replace_type({constr_t, Ann, Con, Types}, Pat, Old, New) ->
    { constr_t, Ann, ?ReplaceType(Con)
    , [?ReplaceType(T) || T <- Types]};
replace_type({field_t, Ann, Id, T}, Pat, Old, New) ->
    {field_t, Ann, Id, ?ReplaceType(T)};
replace_type(Other, _, _Old, _New) ->
    Other.


replace_ast_block(B, Pat, Old, New) when is_list(B) ->
    replace_ast_block(B, Pat, Old, New, []);
replace_ast_block(E, Pat, Old, New) ->
    ?ReplaceAST(E).
replace_ast_block([], _, _, _, Acc) ->
    lists:reverse(Acc);
replace_ast_block([{letval, Ann, Pats, Expr}|Rest], Pat, Old, New, Acc) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    Stmt = {letval, Ann, Pats, ?ReplaceAST(Expr)},
    ?IF(lists:member(Old, PatIds),
        lists:reverse([Stmt|Acc]) ++ Rest,
        replace_ast_block(Rest, Pat, Old, New, [Stmt|Acc])
       );
replace_ast_block([{letfun, Ann, Name, Pats, RT, Expr}|Rest], Pat, Old, New, Acc) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    Stmt = {letfun, Ann, Name, Pats, RT, ?ReplaceAST(Expr)},
    ?IF(lists:member(Old, PatIds),
        lists:reverse([Stmt|Acc]) ++ Rest,
        replace_ast_block(Rest, Pat, Old, New, [Stmt|Acc])
       );
replace_ast_block([{fun_decl, Ann, Name, RT}|Rest], Pat, Old, New, Acc) ->
    replace_ast_block(Rest, Pat, Old, New
                     , [{fun_decl, Ann, Name, ?ReplaceTypeInExpr(RT)}|Acc]);
replace_ast_block([Expr|Rest], Pat, Old, New, Acc) ->
    replace_ast_block(Rest, Pat, Old, New,  [?ReplaceAST(Expr)|Acc]).


replace_type_in_decls(Decls, Pat, Old, New) ->
    [replace_type_in_decl(D, Pat, Old, New) || D <- Decls].

replace_type_in_decl({fun_decl, Ann, Name, RT}, Pat, Old, New) ->
    {fun_decl, Ann, Name, ?ReplaceType(RT)};
replace_type_in_decl({letfun, Ann, Name, Args, RT, Body}, Pat, Old, New) ->
    { letfun, Ann, Name
    , [ {arg, AAnn, AName, ?ReplaceType(AType)} || {arg, AAnn, AName, AType} <- Args]
    , ?ReplaceType(RT)
    , ?ReplaceAST(Body)
    };
replace_type_in_decl({type_def, Ann, Name, TArgs, TDef}, Pat, Old, New) ->
    {type_def, Ann, Name, TArgs, ?ReplaceType(TDef)};
replace_type_in_decl(Other, _, _, _) ->
    Other.
