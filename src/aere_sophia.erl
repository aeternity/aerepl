-module(aere_sophia).

-export([ typecheck/1, parse/2, compile_contract/3
        , parse_body/1, parse_letdef/1, parse_type/1, type_of/2
        ]).

-include("aere_repl.hrl").

-spec typecheck(aeso_syntax:ast()) -> {ok, {aeso_syntax:ast(), aeso_syntax:type()}} | {error, string()}.
typecheck(Ast) ->
    try aeso_ast_infer_types:infer(Ast, []) of
        Res ->
            {ok, Res}
    catch _:{error, Errs} ->
            {error, lists:flatten([What ++ When || {err, _, type_error, What, When} <- Errs, is_list(When)]
                                  ++ [What || {err, _, type_error, What, none} <- Errs])}
    end.

type_of(TypedAst, Name) ->
    {ok, _, Type} = get_decode_type(Name, TypedAst),
    Type.

-spec compile_contract(aevm | fate, string(), aeso_syntax:ast()) -> any.
compile_contract(fate, Src, TypedAst) ->
    FCode    = aeso_ast_to_fcode:ast_to_fcode(TypedAst, []),
    Fate     = aeso_fcode_to_fate:compile(FCode, []),
    ByteCode = aeb_fate_code:serialize(Fate, []),
    {ok, #{byte_code => ByteCode,
            contract_source => Src,
            type_info => [],
            fate_code => Fate,
            compiler_version => aere_version:sophia_version(fate),
            abi_version => aere_version:abi_version(fate),
            payable => maps:get(payable, FCode)
           }};
compile_contract(aevm, Src, TypedAst) ->
    Icode = aeso_ast_to_icode:convert_typed(TypedAst, []),
    TypeInfo  = extract_type_info(Icode),
    Assembler = aeso_icode_to_asm:convert(Icode, []),
    ByteCodeList = to_bytecode(Assembler, []),
    ByteCode = << << B:8 >> || B <- ByteCodeList >>,
    {ok, Version} = aeso_compiler:version(),
    {ok, #{byte_code => ByteCode,
            compiler_version => Version,
            contract_source => Src,
            type_info => TypeInfo,
            abi_version => aeb_aevm_abi:abi_version(),
            payable => maps:get(payable, Icode)
           }}.

get_decode_type(FunName, [{contract, _, _, Defs}]) ->
    GetType = fun({letfun, _, {id, _, Name}, Args, Ret, _})               when Name == FunName -> [{Args, Ret}];
                 ({fun_decl, _, {id, _, Name}, {fun_t, _, _, Args, Ret}}) when Name == FunName -> [{Args, Ret}];
                 (_) -> [] end,
    case lists:flatmap(GetType, Defs) of
        [{Args, Ret}] -> {ok, Args, Ret};
        []            ->
            case FunName of
                "init" -> {ok, [], {tuple_t, [], []}};
                _ -> {error, missing_function}
            end
    end;
get_decode_type(FunName, [_ | Contracts]) ->
    get_decode_type(FunName, Contracts).


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

parse(Parser, I) ->
    case Parser(I) of
        Res = {ok, _} ->
            Res;
        {error, {_, parse_error, Msg}} ->
            {error, io_lib:format("~s", [Msg])};
        {error, {Pos, scan_error}} -> {error, io_lib:format("Scan error at ~p", [Pos])};
        {error, {Pos, scan_error_no_state}} -> {error, io_lib:format("Scan error at ~p", [Pos])};
        {error, {_, ambiguous_parse, As}} -> {error, io_lib:format("Ambiguous parse ~p", [As])};
        {error, {_, include_error, File}} -> {error, io_lib:format("Could not include file ~p", [File])}
    end.

parse_body(I) ->
    parse(fun aeso_parser:body/1, I).
parse_type(I) ->
    parse(fun aeso_parser:type/1, I).
parse_letdef(I) ->
    parse(fun aeso_parser:letdef/1, I).
