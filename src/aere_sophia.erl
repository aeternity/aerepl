-module(aere_sophia).

-export([ typecheck/2, typecheck/1, parse_file/2, parse_file/3, compile_contract/1
        , parse_body/1, parse_top/2
        , parse_decl/1, parse_top/1, parse_type/1, type_of/2
        , generate_interface_decl/1, process_err/1, get_pat_ids/1
        %% , replace_ast/4, replace_type_in_decls/4, replace_type/4
        , replace/4
        ]).

-include("../_build/default/lib/aesophia/src/aeso_parse_lib.hrl").
-include("aere_macros.hrl").

process_err(Errs) when is_list(Errs) ->
    throw({error, lists:concat([aeso_errors:err_msg(E) || E <- Errs])});
process_err(E) -> %% idk, rethrow
    throw(E).


typecheck(Ast) ->
    typecheck(Ast, []).
typecheck(Ast, Opts) ->
    try aeso_ast_infer_types:infer(Ast, Opts) of
        {TypedAst, _, _} ->
            TypedAst;
        {TEnv, TypedAst, _, _} ->
            {TEnv, TypedAst}
    catch _:{error, Errs} ->
              throw({error, process_err(Errs)})
    end.

compile_contract(TypedAst) ->
    {_, FCode} = try aeso_ast_to_fcode:ast_to_fcode(TypedAst, [])
                 catch {error, Ec} -> process_err(Ec) end,
    Fate       = try aeso_fcode_to_fate:compile(FCode, [])
                 catch {error, Ef} -> process_err(Ef) end,
    ByteCode = aeb_fate_code:serialize(Fate, []),
    #{byte_code => ByteCode,
      contract_source => "REPL INPUT",
      type_info => [],
      fate_code => Fate,
      compiler_version => aere_version:sophia_version(),
      abi_version => aere_version:abi_version(),
      payable => maps:get(payable, FCode)
     },
    Fate.

type_of([{contract_main, _, _, _, Defs}], FunName) ->
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
        _ -> error("two entrypoints with the same name")
    end;
type_of([_ | Contracts], FunName) ->
    type_of(Contracts, FunName).

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
parse_file(I, Includes, Opts) ->
    ?with_error_handle(aeso_parser:string(I, Includes, Opts)).


generate_interface_decl([{contract_main, Ann, Name, Funs}]) ->
    {contract_interface, Ann, Name, get_funs_decls(Funs)};
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


%% Replacing:
%%  - type names        - type
%%  - constructors      - con
%%  - identifier names  - var
%%  - any ids           - id
-define(replace(O), replace(O, What, Old, New)).
-define(replace(O, What), replace(O, What, Old, New)).

                                                % Final cases
replace({id, Ann, Old}, id, Old, New) when is_list(New) ->
    {id, Ann, New};
replace({id, _, Old}, id, Old, New) ->
    New;
replace({qid, Ann, Quals}, id, Old, New) when is_list(New) ->
    {qid, Ann, [?IF(Q == Old, New, Q) || Q <- Quals]};
replace(Qid = {qid, _, _}, id, _, _) ->
    Qid;
replace({con, Ann, Old}, id, Old, New) when is_list(New) ->
    {con, Ann, New};
replace({con, _, Old}, id, Old, New) ->
    New;
replace({con, _, Old}, con, Old, New) ->
    New;
replace({id, _, Old}, var, Old, New) ->
    New;

% Dispatch rules
%% Variable scoping
replace(LF = {letfun, Ann, Name, Pats, RT, [{guarded, _, _, Expr}]}, What = var, Old, New) ->
    PatIds = [I || P <- Pats, I <- get_pat_ids(P)],
    ?IF(lists:member(Old, PatIds),
        LF,
        {letfun, Ann, Name, Pats, RT, [{guarded, Ann, [], ?replace(Expr)}]}
       );
replace([LF = {letfun, _, {id, _, N}, _, _, _}|Rest], What = var, Old, New) ->
    LF1 = ?replace(LF),
    ?IF(N == Old, [LF1|Rest], [LF1|?replace(Rest)]);
replace([{letval, Ann, Pat, Expr}|Rest], What = var, Old, New) ->
    PatIds = get_pat_ids(Pat),
    LV1 = {letval, Ann, Pat, ?replace(Expr)},
    [LV1| ?IF(lists:member(Old, PatIds), Rest, ?replace(Rest))];
replace({lam, Ann, Args, Expr}, What = var, Old, New) ->
    ArgIds = [I || {arg, _, I, _} <- Args],
    Expr1 = ?IF(lists:member(Old, ArgIds), Expr, ?replace(Expr)),
    {lam, Ann, Args, Expr1};
%% CONtract vs CONstructor
replace({typed, Ann, X, T}, What = con, Old, New) ->
    {typed, Ann, ?replace(X), T};
replace({ContrOrNs, Ann, Name, Decls}, What = con, Old, New)
  when ContrOrNs =:= contract orelse ContrOrNs =:= namespace ->
    {ContrOrNs, Ann, Name, ?replace(Decls)};
%% God save the field
replace({proj, Ann, Expr, Id}, What = var, Old, New) ->
    {proj, Ann, ?replace(Expr), Id};
replace(Field, _, _, _)
  when element(1, Field) =:= field orelse element(1, Field) =:= field_t ->
    Field;
%% Context-free toplevel function replacement
replace({ContrOrNs, Ann, Name, Decls}, What = var, Old, New)
  when ContrOrNs =:= contract orelse ContrOrNs =:= namespace ->
    {ContrOrNs, Ann, Name, [?replace(X) || X <- Decls]};
%% Types
replace({letfun, Ann, Name, Pats, RT, [{guarded, _, _, Expr}]}, What = type, Old, New) ->
    {letfun, Ann, Name, ?replace(Pats), ?replace(RT, id), [{guarded, Ann, [], ?replace(Expr)}]};
replace({fun_decl, Ann, Name, T}, type, Old, New) ->
    {fun_decl, Ann, Name, ?replace(T, id)};
replace({fun_clauses, Ann, Name, T, Binds}, What = type, Old, New) ->
    {fun_clauses, Ann, Name, ?replace(T, id), [?replace(B) || B <- Binds]};
replace([LF|Rest], What = type, Old, New) when element(1, LF) =:= letfun ->
    [?replace(LF)|?replace(Rest)];
replace([LF|Rest], What = type, Old, New) when element(1, LF) =:= fun_decl ->
    [?replace(LF)|?replace(Rest)];
replace([FCs|Rest], What = type, Old, New) when element(1, FCs) =:= fun_clauses ->
    [?replace(FCs)|?replace(Rest)];
replace([LV|Rest], What = type, Old, New) when element(1, LV) =:= letval ->
    [?replace(LV)|?replace(Rest)];
replace({lam, Ann, Args, Expr}, What = type, Old, New) ->
    Args1 = [{arg, AAnn, AId, ?replace(AType, id)}
             || {arg, AAnn, AId, AType} <- Args],
    {lam, Ann, Args1, ?replace(Expr)};
replace({typed, Ann, X, T}, What = type, Old, New) ->
    {typed, Ann, ?replace(X), ?replace(T, id)};
replace(TDecl, type, Old, New)
  when element(1, TDecl) =:= type_decl
       orelse element(1, TDecl) =:= type_def
       orelse element(1, TDecl) =:= alias_t
       orelse element(1, TDecl) =:= record_t
       orelse element(1, TDecl) =:= variant_t ->
    ?replace(TDecl, id);
replace({ContrOrNs, Ann, Name, Decls}, What = type, Old, New)
  when ContrOrNs =:= contract orelse ContrOrNs =:= namespace ->
    {ContrOrNs, Ann, ?replace(Name, id), [?replace(X) || X <- Decls]};


% General search
replace([], _What, _Old, _New) ->
    [];
replace([H|T], What, Old, New) ->
    [?replace(H)|?replace(T)]; % list comprehension shouldn't be used here
replace(T, What, Old, New) when is_tuple(T) ->
    list_to_tuple([?replace(X) || X <- tuple_to_list(T)]);
replace(Other, _What, _Old, _New) -> Other.

