%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([start/0]).

-record(repl_state,
        { includes :: sets:set(string())
        }).
-type repl_state() :: #repl_state{}.

-type command() ::
        default | parse | quit | eval | type | fcode | fate.

-type command_result() :: {new_state, repl_state()} | finito.

-spec whitespaces() -> string().
whitespaces() ->
    [$\n, $ , $\t, $ ].

-spec init_state() -> repl_state().
init_state() ->
    #repl_state{includes = sets:new()}.
    %% aeso_ast_infer_types:global_env().


-spec start() -> finito.
start() ->
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    repl(init_state()).

-spec repl(repl_state()) -> finito.
repl(State) ->
    Inp = get_input(),
    Res = process_input(State, Inp),
    case Res of
        finito -> finito;
        {new_state, NewState} ->
            repl(NewState)
    end.

-spec commands() -> [{string(), command()}].
commands() ->
    [ {":p", parse}
    , {":q", quit}
    , {":e", eval}
    , {":t", type}
    , {":fcode", fcode}
    , {":fate", fate}
    , {":i", include}
    ].

-spec process_input(repl_state(), string()) -> command_result().
process_input(State, Inp) ->
    Dispatch = fun D([]) ->
                       process_input(State, default, Inp);
                   D([{O, Handle}|Rest]) ->
                       case lists:prefix(O, Inp) of
                          true  -> process_input(State, Handle, string:trim(Inp -- O, leading, whitespaces()));
                          false -> D(Rest)
                       end
               end,
    try Dispatch(commands()) of
        Res ->
            Res
    catch C:[E] ->
            io:format("*** Call threw ~p:~n~p~n", [C, E]),
            erlang:display(erlang:get_stacktrace()),
            {new_state, State};
          C:E ->
            io:format("*** Call threw ~p:~n~p~n", [C, E]),
            erlang:display(erlang:get_stacktrace()),
            {new_state, State}
    end.

-spec process_input(repl_state(), command(), string()) -> command_result().
process_input(State, default, I) ->
    process_input(State, eval, I);
process_input(_, quit, _) ->
    finito;
process_input(State, parse, I) ->
    case aeso_parser:string(I) of
        {ok, Expr} ->
            io:format("~p~n", [Expr]);
        {error, {_, parse_error, Msg}} ->
            io:format("~s~n", [Msg])
    end,
    {new_state, State};
process_input(State, type, I) ->
    case aeso_parser:expr(I) of
        {ok, Expr} ->
            try aeso_ast_infer_types:infer_constant({letval, [], "repl input", {id, [], "_"}, Expr}) of
                Type ->
                    io:format("~s : ~s~n", [ aeso_ast_infer_types:pp_expr("", Expr)
                                           , aeso_ast_infer_types:pp_type("", Type)
                                           ])
            catch
                _:{type_errors, Msg} -> io:format("~s\n", [Msg])
            end;
        {error, {_, parse_error, Msg}} ->
            io:format("~s~n", [Msg])
    end,
    {new_state, State};
process_input(State, fcode, I) ->
    case aeso_parser:expr(I) of
        {ok, Expr} ->
            FC = aeso_ast_to_fcode:constant_to_fexpr(Expr),
            io:format("~s~n", [aeso_ast_to_fcode:format_fexpr(FC)]);
        {error, {_, parse_error, Msg}} ->
            io:format("~s~n", [Msg])
    end,
    {new_state, State};
process_input(State, fate, I) ->
    case aeso_parser:expr(I) of
        {ok, Expr} ->
            FC = aeso_ast_to_fcode:constant_to_fexpr(Expr),
            Fate = aeso_fcode_to_fate:compile_fexpr("REPL", FC),
            io:format("~p~n", [Fate]);
        {error, {_, parse_error, Msg}} ->
            io:format("~s~n", [Msg])
    end,
    {new_state, State};
process_input(State = #repl_state{includes = Includes}, eval, I) ->
    case aeso_parser:expr(I) of
        {ok, Expr} ->
            Libs = unfold_includes(sets:to_list(Includes)),
            Res = eval_contract(I, Libs ++ [mock_contract(Expr)]),
            Res;
        {error, {_, parse_error, Msg}} ->
            io:format("~s~n", [Msg])
    end,
    {new_state, State};
process_input(State = #repl_state{includes = Includes}, include, I) ->
    {new_state, State#repl_state{includes = sets:add_element(I, Includes)}}.


unfold_includes([]) ->
    [];
unfold_includes(Is = [_|_]) ->
    Code = lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, Is),
    {ok, Parsed} = aeso_parser:string(Code),
    Parsed.


-spec get_input() -> string().
get_input() ->
    Line = io:get_line("AESO> "),
    Inp = case Line of
              ":{\n" ->
                  multiline_input();
              _ ->
                  lists:flatten(string:replace(Line, ";", "\n", all))
          end,
    string:trim(Inp, both, whitespaces()).

-spec multiline_input() -> string().
multiline_input() ->
    multiline_input([]).
multiline_input(Acc) ->
    Line = io:get_line("| "),
    case Line of
        ":}\n" -> lists:flatten(lists:reverse(Acc));
        _ -> multiline_input([Line|Acc])
    end.


%%%% Execution

mock_contract(Expr) ->
    {contract, [{file, no_file}], {con, [{file, no_file}], <<"mock_contract">>},
     [{ letfun
      , [{stateful, true}, {entrypoint, true}]
      , {id, [{file, no_file}], "user_input"}
      , []
      , {id, [{file, no_file}, {origin, system}], "_"}
      , Expr}]
    }.

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
    io:format("~p\n", [Resp]).
