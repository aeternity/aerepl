%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([start/0]).

-record(repl_state,
        { includes :: sets:set(string())
        }).

-define(USER_INPUT, "user_input").

init_state() ->
    #repl_state{includes = sets:new()}.
    %% aeso_ast_infer_types:global_env().


start() ->
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    repl(init_state()).

repl(State) ->
    Inp = aere_parse:get_input(),
    case aere_parse:dispatch(Inp) of
        {ok, {Command, Args}} ->
            try process_input(State, Command, Args) of
                {success, Output, State1} ->
                    io:format("~s~n", [Output]),
                    repl(State1);
                skip ->
                    repl(State);
                {error, Err} ->
                    io:format("~s:~n~s~n", [aere_color:red("Error"), Err]),
                    repl(State);
                finito ->
                    finito
            catch C:E ->
                    CommandStr = aere_color:blue(io_lib:format("~p", [Command])),
                    io:format("Call to ~s threw ~p:\n~p\n\nThis is internal error and most likely a bug.\n", [CommandStr, C, E]),
                    repl(State)
            end;
        {error, {no_such_command, Command}} ->
            io:format("No such command ~s\n", [Command]),
            repl(State);
        {error, {ambiguous_prefix, Propositions}} ->
            io:format("Ambiguous command prefix. Matched commands: ~p\n", [Propositions]),
            repl(State)
    end.

process_input(_, quit, _) ->
    finito;
process_input(State, type, I) ->
    todo;
process_input(State, fcode, I) ->
    todo;
process_input(State, fate, I) ->
    todo;
process_input(State, eval, I) ->
    case aeso_parser:expr(I) of
        {ok, Expr} ->
            Res = eval_contract(I, mock_contract(State, Expr)),
            {success, io_lib:format("~p", [Res]), State};
        {error, {_, parse_error, Msg}} ->
            {error, io_lib:format("~s", [Msg])}
    end;
process_input(State = #repl_state{includes = Includes}, include, I) ->
    {success, io_lib:format("Registered module ~s", [I]), State#repl_state{includes = sets:add_element(I, Includes)}}.


unfold_includes(Is) ->
    Code = lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, Is),
    {ok, Parsed} = aeso_parser:string(Code),
    Parsed.

mock_contract(#repl_state{includes = Includes}, Expr) ->
    Libs = unfold_includes(sets:to_list(Includes)),
    Mock = {contract, [{file, no_file}], {con, [{file, no_file}], <<"mock_contract">>},
            [{ letfun
             , [{stateful, true}, {entrypoint, true}]
             , {id, [{file, no_file}], ?USER_INPUT}
             , []
             , {id, [{file, no_file}, {origin, system}], "_"}
             , Expr}]
           },
    Libs ++ [Mock].

typecheck(Ast, Options) ->
    try aeso_ast_infer_types:infer(Ast, Options) of
        TypedAst ->
            {ok, _, Type} = get_decode_type("user_input", TypedAst),
            {TypedAst, Type}
    catch {type_errors, Errs} ->
            {error, aeso_ast_infer_types:pp_error(Errs)}
    end.


compile_contract(fate, Src, Ast) ->
    Options   = [{debug, [scode, opt, opt_rules, compile]}],
    try
        TypedAst = aeso_ast_infer_types:infer(Ast, Options),
        FCode    = aeso_ast_to_fcode:ast_to_fcode(TypedAst, Options),
        Fate     = aeso_fcode_to_fate:compile(FCode, Options),
        ByteCode = aeb_fate_code:serialize(Fate, []),
        {ok, Version}  = aeso_compiler:version(),
        {ok, #{byte_code => ByteCode,
               contract_source => Src,
               type_info => [],
               fate_code => Fate,
               compiler_version => Version,
               abi_version => aeb_fate_abi:abi_version(),
               payable => maps:get(payable, FCode)
              }}
    catch _:E={type_errors, Err} ->
            io:format("~s~n", [Err]),
            E
    end;

compile_contract(aevm, Src, Ast) ->
    Options   = [{debug, [scode, opt, opt_rules, compile]}],
    TypedAst = aeso_ast_infer_types:infer(Ast, Options),
    {ok, _, Type0} = get_decode_type("user_input", TypedAst),
    Icode = aeso_ast_to_icode:convert_typed(TypedAst, Options),
    RetType = aeso_ast_to_icode:ast_typerep(Type0, Icode),
    TypeInfo  = extract_type_info(Icode),
    Assembler = assemble(Icode, Options),
    ByteCodeList = to_bytecode(Assembler, Options),
    ByteCode = << << B:8 >> || B <- ByteCodeList >>,
    {ok, Version} = aeso_compiler:version(),
    {ok, #{byte_code => ByteCode,
           compiler_version => Version,
           contract_source => Src,
           type_info => TypeInfo,
           abi_version => aeb_aevm_abi:abi_version(),
           payable => maps:get(payable, Icode)
          }, RetType}.

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
    %% The __decode should be in the final contract
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

assemble(Icode, Options) ->
    aeso_icode_to_asm:convert(Icode, Options).

to_bytecode(['COMMENT',_|Rest],_Options) ->
    to_bytecode(Rest,_Options);
to_bytecode([Op|Rest], Options) ->
    [aeb_opcodes:m_to_op(Op)|to_bytecode(Rest, Options)];
to_bytecode([], _) -> [].

build_contract(Src, Owner, C, Args, S) ->
    build_contract(Src, Owner, C, Args, #{}, S).

build_contract(Src, Owner, C, Args, Options, S) ->
    case compile_contract(aevm, Src, C) of
        {ok, Code, RetType} ->
            Serialized  = aect_sophia:serialize(Code, aere_version:latest_sophia_contract_version()),
            {aere_runtime:create_contract(Owner, Serialized, Args, Options, S), RetType};
        {error, Reason} ->
            error({fail, {error, compile_should_work, got, Reason}})
    end.

new_state() ->
    #{}.

eval_contract(Src, C) ->
    aere_runtime:state(new_state()),
    S0 = aere_runtime:state(),
    {Acc, S1} = aere_runtime:new_account(100000021370000999, S0),
    {{Con, S2}, RetType} = build_contract(Src, Acc, C, {}, S1),
    {Resp, _} = aere_runtime:call_contract(Acc, Con, user_input, RetType, {}, S2),
    Resp.
