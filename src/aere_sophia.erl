-module(aere_sophia).

-export([ typecheck/2, typecheck/1, parse_file/2, parse_file/3, compile_contract/1
        , parse_body/1, parse_top/2
        , parse_decl/1, parse_top/1, parse_type/1, type_of/2
        , process_err/1
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
        {_, TypedAst, _} ->
            TypedAst;
        {TEnv, _, TypedAst, _} ->
            {TEnv, TypedAst}
    catch _:{error, Errs} ->
              throw({error, process_err(Errs)})
    end.

compile_contract(TypedAst) ->
    {#{child_con_env := ChildConEnv}, FCode}
        = try aeso_ast_to_fcode:ast_to_fcode(TypedAst, [])
          catch {error, Ec} -> process_err(Ec) end,
    Fate
        = try aeso_fcode_to_fate:compile(ChildConEnv, FCode, [])
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
parse_file(I, Includes, Opts) when is_binary(I) ->
    parse_file(binary:bin_to_list(I), Includes, Opts);
parse_file(I, Includes, Opts) ->
    ?with_error_handle(aeso_parser:string(I, Includes, Opts)).
