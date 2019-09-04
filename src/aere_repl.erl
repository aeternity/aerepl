%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([start/0]).

-record(repl_state,
        { include_ast :: list(aeso_ast:ast())
        , include_hashes :: sets:set(aeso_parser:include_hash())
        , include_files :: list(string())
        }).

-define(USER_INPUT, "user_input").

init_state() ->
    #repl_state{ include_ast = []
               , include_hashes = sets:new()
               , include_files = []
               }.
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
                    io:format("~s~n", [aere_color:emph(Output)]),
                    repl(State1);
                skip ->
                    repl(State);
                {error, Err} ->
                    io:format("~s:~n~s~n", [aere_color:red("Error"), aere_color:emph(Err)]),
                    repl(State);
                finito ->
                    finito
            catch error:E ->
                    CommandStr = aere_color:blue(io_lib:format("~p", [Command])),
                    io:format("Command ~s failed:\n" ++ aere_color:red("~p\n"), [CommandStr, E]),
                    io:format("Stacktrace:\n" ++ aere_color:emph("~p") ++"\n\n", [erlang:get_stacktrace()]),
                    io:format(aere_color:emph("*** This is an internal error and most likely a bug.\n")),
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
    case aeso_parser:body(I) of
        {ok, Expr} ->
            Contract = mock_contract(State, Expr),
            case typecheck(Contract) of
                {error, _} = E -> E;
                {ok, {_, Type}} ->
                    {success, aeso_ast_infer_types:pp_type("", Type), State}
            end;
        {error, {_, parse_error, Msg}} ->
            {error, Msg}
    end;
process_input(State, eval, I) ->
    case aeso_parser:body(I) of
        {ok, Expr} ->
            case eval_contract(I, mock_contract(State, Expr)) of
                {ok, Res} -> {success, io_lib:format("~s", [Res]), State};
                {error, Msg} = E when is_list(Msg) ->
                    E
            end;
        {error, {_, parse_error, Msg}} ->
            {error, io_lib:format("~s", [Msg])}
    end;
process_input(State, include, Inp) ->
    Files = string:tokens(Inp, aere_parse:whitespaces()),
    register_includes(State, Files);
process_input(State = #repl_state{include_files = IFiles}, reinclude, _) ->
    register_includes(State#{ include_ast := []
                            , include_hashes := sets:new()
                            , include_files := []
                            }, IFiles);
process_input(State, uninclude, _) ->
    {success, "Unregistered all includes",
     State#repl_state{ include_ast = []
                     , include_hashes = sets:new()
                     , include_files = []
                     }}.

register_includes(State = #repl_state{ include_ast = Includes
                                     , include_hashes = Hashes
                                     , include_files = PrevFiles
                                     }
                 , Files) ->
    case Files -- (Files -- PrevFiles) of
        [] ->
            IncludingContract = lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, Files),
            case aeso_parser:string(IncludingContract, Hashes, [keep_included]) of
                {ok, {Addition, NewHashes}} ->
                    NewIncludes = Includes ++ Addition,
                    try aeso_ast_infer_types:infer(NewIncludes, []) of
                        _ ->
                            Colored = aere_color:yellow(lists:flatten([" " ++ F || F <- Files])),
                            IncludeWord = case Files of
                                              []  -> "nothing";
                                              [_] -> "include";
                                              _   -> "includes"
                                          end,
                            {success, "Registered " ++ IncludeWord ++ Colored,
                             State#repl_state{ include_ast = NewIncludes
                                             , include_hashes = NewHashes
                                             , include_files = Files ++ PrevFiles
                                             }
                            }
                    catch _:{type_errors, Errs} ->
                            {error, Errs}
                    end;
                {error, {Pos, scan_error}} ->
                    {error, io_lib:format("Scan error at ~p", [Pos])};
                {error, {Pos, scan_error_no_state}} ->
                    {error, io_lib:format("Scan error at ~p", [Pos])};
                {error, {_, parse_error, Error}} ->
                    {error, io_lib:format("Parse error:\n~s", [Error])};
                {error, {_, ambiguous_parse, As}} ->
                    {error, io_lib:format("Ambiguous parse:\n~p", [As])};
                {error, {_, include_error, File}} ->
                    {error, io_lib:format("Could not find ~p", [File])}
            end;
        Duplicates ->
            Colored = aere_color:yellow(lists:flatten([" " ++ D || D <- Duplicates])),
            {error, io_lib:format("Following imports are already included: ~s", [Colored])}
    end.


mock_contract(#repl_state{include_ast = Includes}, Expr) ->
    Mock = {contract, [{file, no_file}], {con, [{file, no_file}], <<"mock_contract">>},
            [{ letfun
             , [{stateful, true}, {entrypoint, true}]
             , {id, [{file, no_file}], ?USER_INPUT}
             , []
             , {id, [{file, no_file}, {origin, system}], "_"}
             , Expr}]
           },
    Includes ++ [Mock].


typecheck(Ast) ->
    typecheck(Ast, []).
typecheck(Ast, Options) ->
    try aeso_ast_infer_types:infer(Ast, Options) of
        Res ->
            TypedAst = case Res of
                           {_Env, TAst} -> TAst;
                           _ -> Res
                       end,
            {ok, _, Type} = get_decode_type(?USER_INPUT, TypedAst),
            {ok, {Res, Type}}
    catch _:{type_errors, Errs} ->
            {error, Errs}
    end.


%% compile_contract(fate, Src, Ast) ->
%%     Options   = [{debug, [scode, opt, opt_rules, compile]}],
%%     try
%%         TypedAst = aeso_ast_infer_types:infer(Ast, Options),
%%         FCode    = aeso_ast_to_fcode:ast_to_fcode(TypedAst, Options),
%%         Fate     = aeso_fcode_to_fate:compile(FCode, Options),
%%         ByteCode = aeb_fate_code:serialize(Fate, []),
%%         {ok, Version}  = aeso_compiler:version(),
%%         {ok, #{byte_code => ByteCode,
%%                contract_source => Src,
%%                type_info => [],
%%                fate_code => Fate,
%%                compiler_version => Version,
%%                abi_version => aeb_fate_abi:abi_version(),
%%                payable => maps:get(payable, FCode)
%%               }}
%%     catch _:E={type_errors, Err} ->
%%             io:format("~s~n", [Err]),
%%             E
%%     end;

compile_contract(aevm, Src, Ast) ->
    Options   = [{debug, [scode, opt, opt_rules, compile]}],
    case typecheck(Ast, Options) of
        {error, _} = E -> E;
        {ok, {TypedAst, ExprType}} ->
            Icode = aeso_ast_to_icode:convert_typed(TypedAst, Options),
            RetType = aere_response:convert_type(build_type_map(TypedAst), ExprType),
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
                  }, RetType}
    end.

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
        {error, _} = E -> E
    end.

new_state() ->
    #{}.

eval_contract(Src, C) ->
    aere_runtime:state(new_state()),
    S0 = aere_runtime:state(),
    {Acc, S1} = aere_runtime:new_account(100000021370000999, S0),
    try build_contract(Src, Acc, C, {}, S1) of
        {{Con, S2}, RetType} ->
            {Resp, _} = aere_runtime:call_contract(Acc, Con, user_input, RetType, {}, S2),
            {ok, prettypr:format(aere_response:pp_response(Resp))};
        {error, _} = E -> E
    catch
        error:{code_errors, E} ->
            {error, io_lib:format("~p", [E])}
    end.

build_type_map(Ast) ->
    build_type_map([], Ast, #{}).
build_type_map(_Scope, [], Acc) ->
    Acc;
build_type_map(Scope, [{namespace, _, {con, _, Name}, Defs} | Rest], Acc) ->
    build_type_map(Scope, Rest, build_type_map(Scope ++ [Name], Defs, Acc));
build_type_map(Scope, [{type_def, _, {id, _, Name}, Args, {variant_t, Cons}} | Rest], Acc) ->
    build_type_map(Scope, Rest, Acc#{Scope ++ [Name] => {variant, Args, Cons}});
build_type_map(Scope, [{type_def, _, {id, _, Name}, Args, {record_t, Fields}} | Rest], Acc) ->
    build_type_map(Scope, Rest, Acc#{Scope ++ [Name] => {record, Args, Fields}});
build_type_map(Scope, [_|Rest], Acc) ->
    build_type_map(Scope, Rest, Acc).

