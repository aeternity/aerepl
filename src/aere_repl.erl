%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([start/0]).

-include("aere_repl.hrl").

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
               , chain_state = new_state()
               , user_contract_state_type = {tuple_t, [], []}
               , user_contracts = []
               , tracked_contracts = #{}
               , let_defs = #{}
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
                    repl(State);
                  X ->
                    CommandStr = aere_color:blue(io_lib:format("~p", [Command])),
                    io:format("Command ~s failed with\n~p\n", [CommandStr, X]),
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
                {options, Opts#options{Field = true}};
            Val =:= "false" orelse Val =:= 0 ->
                {options, Opts#options{Field = false}};
            true ->
                {error, "true/false value expected"}
        end).
-define(ParseOptionInt(Field),
        try {options, Opts#options{Field = list_to_integer(Val)}}
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
            Contract = State#repl_state.include_ast ++
                [aere_mock:contract([aere_mock:entrypoint(?USER_INPUT, Expr, full)])],
            case aere_sophia:typecheck(Contract) of
                {error, _} = E -> E;
                {ok, {_, Type}} ->
                    {success, aeso_ast_infer_types:pp_type("", Type), State}
            end;
        {error, {_, parse_error, Msg}} ->
            {error, Msg}
    end;
process_input(State = #repl_state{ user_contract_state_type = StateType
                                 , user_contracts = PrevContracts
                                 }, eval, I) ->
    case aere_sophia:parse_body(I) of
        {ok, Expr} ->
            Mock = case StateType of
                       {id, _, "unit"} ->
                           State#repl_state.include_ast ++
                               [aere_mock:contract([aere_mock:entrypoint(?USER_INPUT, Expr, full)])];
                       {tuple_t, _, []} ->
                           State#repl_state.include_ast ++
                               [aere_mock:contract([aere_mock:entrypoint(?USER_INPUT, Expr, full)])];
                       _ -> case PrevContracts of
                                [] -> State#repl_state.include_ast ++ [aere_mock:contract(Expr)];
                                [Prev|_] ->
                                    PrevDecl = aere_mock:contract( ?PREV_CONTRACT
                                                                 , [aere_mock:decl(?GET_STATE, StateType)]),
                                    Init = {app,[],
                                            {proj, aere_mock:ann(),
                                             {contract_pubkey, aere_mock:ann(), Prev},
                                             {id, aere_mock:ann(), ?GET_STATE}
                                            }, []},
                                    State#repl_state.include_ast ++
                                        [PrevDecl,
                                         aere_mock:contract([aere_mock:typedef("state", StateType),
                                                        aere_mock:entrypoint("init", Init),
                                                        aere_mock:entrypoint(?USER_INPUT, Expr),
                                                        aere_mock:entrypoint(?GET_STATE, {id, [], "state"})])]
                            end
                   end,
            case eval_contract(I, Mock, State) of
                {ok, {NewState, Res}} -> {success, io_lib:format("~s", [Res]), NewState};
                {error, Msg} = E when is_list(Msg) ->
                    E
            end;
        Er -> Er
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
process_input(State = #repl_state{options = Opts, include_ast = Includes}, set, Inp) ->
    {Prop, Val0} = lists:splitwith(fun(X) -> X /= $  end, Inp),
    Val = string:trim(Val0),
    Parse =
        case Prop of
            "call_gas" -> ?ParseOptionBool(display_call_gas);
            "deploy_gas" -> ?ParseOptionBool(display_deploy_gas);
            "gas" -> ?ParseOptionInt(gas);
            "value" -> ?ParseOptionInt(call_value);
            "aevm" -> { options, "Not supported at all, use at your own responsibility"
                      , Opts#options{backend = aevm}};
            "fate" -> {options, Opts#options{backend = fate}};
            "state" ->
                case aere_sophia:parse_body(Val) of
                    {ok, Expr} ->
                        case aere_sophia:typecheck(
                               Includes ++
                                   [aere_mock:contract([aere_mock:entrypoint(?USER_INPUT, Expr, full)])]) of
                            {ok, {_, Type}} ->
                                Mock = Includes ++
                                    [aere_mock:contract(
                                       [ aere_mock:typedef("state", Type)
                                       , aere_mock:entrypoint("init", Expr)])],
                                S0 = State#repl_state.chain_state,
                                case build_deploy_contract("no_src", Mock, {}, Opts, S0) of
                                    {{Key, _, _}, S1} ->
                                        {state, State#repl_state
                                         { user_contract_state_type = Type
                                         , user_contracts = [Key]
                                         , chain_state = S1
                                         }};
                                    {error, _} = REr -> REr
                                end;
                            {error, _} = TEr -> TEr
                        end;
                    {error, _} = PEr -> PEr
                end;
            _ -> {error, "Unknown property"}
        end,
    case Parse of
        {error, _} = E ->
            E;
        {options, NewOpts} ->
            {success, State#repl_state{options = NewOpts}};
        {state, NewState} ->
            {success, NewState};
        {options, Msg, NewOpts} ->
            {success, Msg, State#repl_state{options = NewOpts}};
        {state, Msg, NewState} ->
            {success, Msg, NewState}
    end;
process_input(State = #repl_state{ tracked_contracts = Contracts
                                 , chain_state = S0
                                 , options = Opts
                                 }, deploy, Inp) ->
    Pars =
        case string:tokens(Inp, aere_parse:whitespaces()) of
            [] -> {error, "What to deploy? Give me some file"};
            [F] -> {ok, {F, io_lib:format("con~p", [maps:size(Contracts)])}};
            [F, "as", N] -> {ok, {F, N}};
            _ -> {error, "Bad input format"}
        end,
    case Pars of
        {ok, {File, Name}} ->
            case file:read_file(File) of
                {ok, Src} ->
                    case aere_sophia:parse_body(Src) of
                        {ok, Ast} ->
                            aere_runtime:state(S0),
                            case build_deploy_contract(Src, Ast, {}, Opts, S0) of
                                {{Con, _, _}, S1} ->
                                    {success, "Contract deployed as " ++ aere_color:yellow(Name),
                                     State#repl_state{ tracked_contracts = Contracts#{Name => Con}
                                                     , chain_state = S1
                                                     }
                                    };
                                {error, _} = DEr -> DEr
                            end;
                        {error, _} = PEr -> PEr
                    end;
                {error, _} -> {error, "Could not load file " ++ aere_color:yellow(Name)}
            end;
        {error, _} = E -> E
    end;
%% process_input(State, 'let', Inp) ->
%%     case parse_letdef(Inp) of
%%         {ok, Def = {letval, _, {id, _, Name}, _, Body}} ->
%%             case typecheck()
process_input(_, _, _) ->
    {error, "This command is not defined yet."}.


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
                             State#repl_state{ include_ast    = NewIncludes
                                             , include_hashes = NewHashes
                                             , include_files  = Files ++ PrevFiles
                                             }
                            }
                    catch _:{type_errors, Errs} ->
                            {error, Errs}
                    end
            catch {error, {Pos, scan_error}} ->
                    {error, io_lib:format("Scan error at ~p", [Pos])};
                  {error, {Pos, scan_error_no_state}} ->
                    {error, io_lib:format("Scan error at ~p", [Pos])};
                  {error, {_, ambiguous_parse, As}} ->
                    {error, io_lib:format("Ambiguous parse:\n~p", [As])};
                  {error, {_, include_error, File}} ->
                    {error, io_lib:format("Could not find ~p", [File])};
                  {error, Errs} when is_list(Errs) ->
                    {error, lists:flatten([io_lib:format("Parse error at ~p: ~s\n", [Pos, Er]) ||
                                              {err, Pos, parse_error, Er, _} <- Errs
                                          ])}
            end;
        Duplicates ->
            Colored = aere_color:yellow(lists:flatten([" " ++ D || D <- Duplicates])),
            {error, io_lib:format("Following imports are already included: ~s", [Colored])}
    end.


build_deploy_contract(Src, Ast, Args, Options = #options{backend = Backend}, S0) ->
    case aere_sophia:typecheck(Ast) of
        {ok, {TypedAst, ExprType}} ->
            RetType = aere_response:convert_type(build_type_map(TypedAst), ExprType),
            try aere_sophia:compile_contract(Backend, Src, TypedAst) of
                {ok, Code} ->
                    Serialized = aect_sophia:serialize(Code, aere_version:contract_version()),
                    {Owner, S1} = aere_runtime:new_account(100000021370000999, S0),
                    try aere_runtime:create_contract(Owner, Serialized, Args, Options, S1) of
                        {{Con, Gas}, S2} ->
                            {{Con, Gas, RetType}, S2}
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
                    {error, lists:flatten([What ++ When ||
                                              {err, _, code_error, What, When} <- Errs, is_list(When)]
                                          ++ [What || {err, _, code_error, What, none} <- Errs])}
            end;
        {error, _} = E -> E
    end.

new_state() ->
    #{}.

eval_contract(Src, C, State = #repl_state{options = Options}) ->
    S0 = State#repl_state.chain_state,
    aere_runtime:state(S0),
    case build_deploy_contract(Src, C, {}, Options, S0) of
        {{Con, GasDeploy, RetType}, S1} ->
            {Owner, S2} = aere_runtime:new_account(100000021370000999, S1),
            {{Resp, GasCall}, S3} =
                aere_runtime:call_contract(Owner, Con, list_to_binary(?USER_INPUT), RetType, {}, Options, S2),

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

            {ok, { State#repl_state{ user_contracts = [Con|State#repl_state.user_contracts]
                                   , chain_state = S3}
                 , lists:concat([PPResp, DeployGasStr, CallGasStr])}};
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

