%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([start/0]).

-include("aere_repl.hrl").

-define(USER_INPUT, "user_input").

-spec default_options() -> options().
default_options() ->
    #options{ display_call_gas = false
            , display_deploy_gas = false
            , gas = 1000000
            , height = 1
            , call_value = 0
            , backend = fate
            }.

-spec init_state() -> repl_state().
init_state() ->
    #repl_state{ include_ast = []
               , include_hashes = sets:new()
               , include_files = []
               , options = default_options()
               }.

-spec start() -> finito.
start() ->
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    repl(init_state()).

-spec repl(repl_state()) -> finito.
repl(State) ->
    Inp = aere_parse:get_input(),
    case aere_parse:dispatch(Inp) of
        skip ->
            repl(State);
        {ok, {Command, Args}} ->
            try process_input(State, Command, Args) of
                {success, Output, State1} ->
                    io:format("~s~n", [aere_color:emph(Output)]),
                    repl(State1);
                {success, State1} ->
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
            io:format("No such command " ++ aere_color:blue("~p") ++ "\n", [Command]),
            repl(State);
        {error, {ambiguous_prefix, Propositions}} ->
            PropsString =
                aere_color:blue(lists:flatten([io_lib:format(" ~p", [P]) || P <- Propositions])),
            io:format("Ambiguous command prefix. Matched commands: ~s\n", [PropsString]),
            repl(State)
    end.


-define(ParseOptionBool(Field),
        if
            Val =:= "true" orelse Val =:= 1 ->
                Opts#options{Field = true};
            Val =:= "false" orelse Val =:= 0 ->
                Opts#options{Field = false};
            true ->
                {error, "true/false value expected"}
        end).
-define(ParseOptionInt(Field),
        try Opts#options{Field = list_to_integer(Val)}
        catch error:badarg -> {error, "integer value expected"}
        end).
-spec process_input(repl_state(), aere_parse:command(), string()) ->
                           finito | {error, string()} | {success, string(), repl_state()}
                               | {success, repl_state()}.
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
            case eval_contract(I, mock_contract(State, Expr), State#repl_state.options) of
                {ok, Res} -> {success, io_lib:format("~s", [Res]), State};
                {error, Msg} = E when is_list(Msg) ->
                    E
            end;
        {error, {_, parse_error, Msg}} ->
            {error, io_lib:format("~s", [Msg])};
        {error, {Pos, scan_error}} -> {error, io_lib:format("Scan error at ~p", [Pos])};
        {error, {Pos, scan_error_no_state}} -> {error, io_lib:format("Scan error at ~p", [Pos])};
        {error, {_, ambiguous_parse, As}} -> {error, io_lib:format("Ambiguous parse ~p", [As])};
        {error, {_, include_error, File}} -> {error, io_lib:format("Could not include file ~p", [File])}
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
                     }};
process_input(State = #repl_state{options = Opts}, set, Inp) ->
    {Prop, Val0} = lists:splitwith(fun(X) -> X /= $  end, Inp),
    Val = string:trim(Val0),
    Parse =
        case Prop of
            "call_gas" -> ?ParseOptionBool(display_call_gas);
            "deploy_gas" -> ?ParseOptionBool(display_deploy_gas);
            "gas" -> ?ParseOptionInt(gas);
            "value" -> ?ParseOptionInt(call_value);
            "aevm" -> Opts#options{backend = aevm};
            "fate" -> Opts#options{backend = fate};
            _ -> {error, "Unknown property"}
        end,
    case Parse of
        {error, _} = E ->
            E;
        NewOpts = #options{} ->
            {success, State#repl_state{options = NewOpts}}
    end.

-spec register_includes(repl_state(), list(string())) -> repl_state().
register_includes(State = #repl_state{ include_ast = Includes
                                     , include_hashes = Hashes
                                     , include_files = PrevFiles
                                     }
                 , Files) ->
    case Files -- (Files -- PrevFiles) of
        [] ->
            IncludingContract = lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, Files),
            try aeso_parser:string(IncludingContract, Hashes, [keep_included]) of
                {Addition, NewHashes} ->
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
                    end
            catch {error, {Pos, scan_error}} ->
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

-spec mock_contract(repl_state(), aeso_syntax:expr()) -> aeso_syntax:ast().
mock_contract(#repl_state{include_ast = Includes}, Body) ->
    Mock = {contract, [{file, no_file}], {con, [{file, no_file}], <<"mock_contract">>},
            [{ letfun
             , [{stateful, true}, {entrypoint, true}]
             , {id, [{file, no_file}], ?USER_INPUT}
             , []
             , {id, [{file, no_file}, {origin, system}], "_"}
             , Body}]
           },
    Includes ++ [Mock].

-spec typecheck(aeso_syntax:ast()) -> {ok, {aeso_syntax:ast(), aeso_syntax:type()}} | {error, string()}.
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
    catch _:{error, Errs} ->
            {error, lists:flatten([What ++ When || {err, _, type_error, What, When} <- Errs, is_list(When)]
                                  ++ [What || {err, _, type_error, What, none} <- Errs])}
    end.


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

build_contract(Src, Owner, Ast, Args, Options = #options{backend = Backend}, S) ->
    case typecheck(Ast) of
        {ok, {TypedAst, ExprType}} ->
            RetType  = aere_response:convert_type(build_type_map(TypedAst), ExprType),
            try compile_contract(Backend, Src, TypedAst) of
                {ok, Code} ->
                    Serialized  = aect_sophia:serialize(Code, aere_version:contract_version()),
                    try aere_runtime:create_contract(Owner, Serialized, Args, Options, S) of
                        {{Con, Gas}, S1} ->
                            {{Con, Gas, RetType}, S1}
                    catch error:{failed_contract_create, Reason} ->
                            ReasonS = if is_binary(Reason) -> binary_to_list(Reason);
                                         is_list(Reason) -> Reason;
                                         true -> io_lib:format("~p", [Reason])
                                      end,
                            {error, ReasonS}
                    end;
                {error, _} = E -> E
            catch
                {error, Errs} when is_list(Errs) ->
                    {error, lists:flatten([What ++ When || {err, _, code_error, What, When} <- Errs, is_list(When)]
                                          ++ [What || {err, _, code_error, What, none} <- Errs])}
            end;
        {error, _} = E -> E
    end.

new_state() ->
    #{}.

eval_contract(Src, C, Options) ->
    aere_runtime:state(new_state()),
    S0 = aere_runtime:state(),
    {Acc, S1} = aere_runtime:new_account(100000021370000999, S0),
    case build_contract(Src, Acc, C, {}, Options, S1) of
        {{Con, GasDeploy, RetType}, S2} ->
            {{Resp, GasCall}, _} = aere_runtime:call_contract(Acc, Con, user_input, RetType, {}, Options, S2),
            PPResp = prettypr:format(aere_response:pp_response(Resp)),
            DeployGasStr =
                case Options#options.display_deploy_gas of
                    true -> lists:concat([aere_color:yellow("\ndeploy gas"), ": ", GasDeploy]);
                    false -> ""
                end,
            CallGasStr =
                case Options#options.display_call_gas of
                    true -> lists:concat([aere_color:yellow("\ncall gas"), ": ", GasCall]);
                    false -> ""
                end,
            {ok, lists:concat([PPResp, DeployGasStr, CallGasStr])};
        {error, _} = E -> E
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

