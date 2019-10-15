-module(aere_sophia).

-export([ typecheck/1, parse_file/2, parse_file/3, compile_contract/3
        , parse_body/1, parse_letdef/1, parse_type/1, type_of/2
        , generate_interface_decl/1, process_err/1
        ]).

-include("aere_repl.hrl").

process_err(Errs) when is_list(Errs) ->
    throw({error, lists:concat([aeso_errors:err_msg(E) || E <- Errs])});
process_err(E) -> %% idk, rethrow
    throw(E).


typecheck(Ast) ->
    try aeso_ast_infer_types:infer(Ast, [])
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
parse_body(I) ->
    ?with_error_handle(aeso_parser:body(I)).
parse_type(I) ->
    ?with_error_handle(aeso_parser:type(I)).
parse_letdef(I) ->
    ?with_error_handle(aeso_parser:letdef(I)).
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
    TArgs = [T || {arg, _, _, T} <- Args],
    get_funs_decls(Rest, [{fun_decl, Ann, Name, {fun_t, Ann, [], TArgs, RetType}}|Acc]);
get_funs_decls([Decl | Rest], Acc) when element(1, Decl) =:= fun_decl
                                        orelse element(1, Decl) =:= type_decl
                                        orelse element(1, Decl) =:= type_def
                                        ->
    get_funs_decls(Rest, [Decl | Acc]).



