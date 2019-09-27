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
               , chain_state = #{}
               , user_contract_state_type = {tuple_t, [], []}
               , user_contracts = []
               , tracked_contracts = #{}
               , let_defs = []
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
                  {error, Err} ->
                    io:format("~s:~n~s~n", [aere_color:red("Error"), aere_color:emph(Err)]),
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
                throw({error, "true/false value expected"})
        end).
-define(ParseOptionInt(Field),
        try {options, Opts#options{Field = list_to_integer(Val)}}
        catch error:badarg -> throw({error, "integer value expected"})
        end).
-spec process_input(repl_state(), aere_parse:command(), string()) ->
                           finito | {error, string()} | {success, string(), repl_state()}
                               | {success, repl_state()}.
process_input(_, quit, _) ->
    finito;
process_input(State, type, I) ->
    Expr = aere_sophia:parse_body(I),
    Contract = aere_mock:chained_query_contract(State, Expr),
    TAst = aere_sophia:typecheck(Contract),
    {_, Type} = aere_sophia:type_of(TAst, ?USER_INPUT),
    {success, aeso_ast_infer_types:pp_type("", Type), State};
process_input(State, eval, I) ->
    Expr = aere_sophia:parse_body(I),
    Mock = aere_mock:chained_query_contract(State, Expr),
    {NewState, Res} = eval_contract(I, Mock, State),
    {success, io_lib:format("~s", [Res]), NewState};
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
            "aevm" -> { options, "Not supported at all, use at your own responsibility"
                      , Opts#options{backend = aevm}};
            "fate" -> {options, Opts#options{backend = fate}};
            "state" ->
                Expr = aere_sophia:parse_body(Val),
                TExprAst = aere_sophia:typecheck(aere_mock:simple_query_contract(State, Expr)),
                {_, Type} = aere_sophia:type_of(TExprAst, ?USER_INPUT),
                Mock = aere_mock:chained_initial_contract(State, Expr, Type),
                TMock = aere_sophia:typecheck(Mock),
                S0 = State#repl_state.chain_state,
                {{Key, _}, S1} = build_deploy_contract("no_src", TMock, {}, Opts, S0),
                {state, State#repl_state
                 { user_contract_state_type = Type
                 , user_contracts = [Key]
                 , chain_state = S1
                 }};
            _ -> throw({error, "Unknown property"})
        end,
    case Parse of
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
    {File, Name} =
        case string:tokens(Inp, aere_parse:whitespaces()) of
            [] -> throw({error, "What to deploy? Give me some file"});
            [F] -> {F, io_lib:format("con~p", [maps:size(Contracts)])};
            [F, "as", N] -> {F, N};
            _ -> throw({error, "Bad input format"})
        end,
    case file:read_file(File) of
        {ok, Src} ->
            Ast = aere_sophia:parse_body(Src),
            TAst = aere_sophia:typecheck(Ast),
            {{Con, _}, S1} = build_deploy_contract(Src, TAst, {}, Opts, S0),
            {success, "Contract deployed as " ++ aere_color:yellow(Name),
             State#repl_state{ tracked_contracts = Contracts#{Name => Con}
                             , chain_state = S1
                             }
            };
        {error, _} -> throw({error, "Could not load file " ++ aere_color:yellow(Name)})
    end;
process_input(State = #repl_state{ options = Opts
                                 , chain_state = S0
                                 }, 'let', Inp) ->
    case aere_sophia:parse_letdef(Inp) of
        {letval, _, {id, _, Name}, _, Body} ->
            Provider = aere_mock:letval_provider(unregister_letdef(State, Name), Name, Body),
            TProvider = aere_sophia:typecheck(Provider),
            {_, Type} = aere_sophia:type_of(TProvider, Name),
            {{Con, _}, S1} = build_deploy_contract(Inp, TProvider, {}, Opts, S0),
            NewState = register_letdef( State#repl_state{ chain_state = S1}
                                      , Name, {letval, Con, Type}),
            {success, NewState};
        {letfun, _, {id, _, Name}, Args, _, Body} ->
            NewState = register_letdef(State, Name, {letfun, Args, Body}),
            aere_sophia:typecheck(
              aere_mock:with_prelude(NewState,
                aere_mock:contract(
                  [aere_mock:entrypoint(?USER_INPUT, {id, aere_mock:ann(), "state"})]))),
            {success, NewState}
    end;
process_input(_, _, _) ->
    {error, "This command is not defined yet."}.

unregister_letdef(State = #repl_state{let_defs = LetDefs}, Name) ->
    State#repl_state{let_defs = proplists:delete(Name, LetDefs)}.

register_letdef(State = #repl_state{let_defs = LetDefs}, Name, Def) ->
    [ throw({error, "Function redefinition is not supported"})
      || {N, {letfun, _, _}} <- LetDefs, N == Name],
    State#repl_state{let_defs = proplists:delete(Name, LetDefs) ++ [{Name, Def}]}.

-spec register_includes(repl_state(), list(string())) -> repl_state().
register_includes(State = #repl_state{ include_ast = Includes
                                     , include_hashes = Hashes
                                     , include_files = PrevFiles
                                     }
                 , Files) ->
    case Files -- (Files -- PrevFiles) of
        [] ->
            IncludingContract = lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, Files),
            {Addition, NewHashes} = aere_sophia:parse_file(IncludingContract, Hashes, [keep_included]),
            NewIncludes = Includes ++ Addition,
            aere_sophia:typecheck(NewIncludes),
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
            };
        Duplicates ->
            Colored = aere_color:yellow(lists:flatten([" " ++ D || D <- Duplicates])),
            {error, io_lib:format("Following files are already included: ~s", [Colored])}
    end.


build_deploy_contract(Src, TypedAst, Args, Options = #options{backend = Backend}, S0) ->
    Code = aere_sophia:compile_contract(Backend, Src, TypedAst),
    aere_runtime:state(S0),
    Serialized = aect_sophia:serialize(Code, aere_version:contract_version()),
    {Owner, S1} = aere_runtime:new_account(100000021370000999, S0),
    try aere_runtime:create_contract(Owner, Serialized, Args, Options, S1)
    catch error:{failed_contract_create, Reason} ->
            ReasonS = if is_binary(Reason) -> binary_to_list(Reason);
                         is_list(Reason) -> Reason;
                         true -> io_lib:format("~p", [Reason])
                      end,
            throw({error, ReasonS})
    end.


eval_contract(Src, Ast, State = #repl_state{options = Options}) ->
    io:format("~p~n~n", [Ast]),
    TypedAst = aere_sophia:typecheck(Ast),
    RetType = aere_response:convert_type( build_type_map(TypedAst)
                                        , element(2, aere_sophia:type_of(TypedAst, ?USER_INPUT))),
    S0 = State#repl_state.chain_state,
    {{Con, GasDeploy}, S1} = build_deploy_contract(Src, TypedAst, {}, Options, S0),
    {Owner, S2} = aere_runtime:new_account(100000021370000999, S1),
    {{Resp, GasCall}, S3} =
        aere_runtime:call_contract( Owner, Con, list_to_binary(?USER_INPUT)
                                  , RetType, {}, Options, S2),

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
    { State#repl_state{ user_contracts = [Con|State#repl_state.user_contracts]
                      , chain_state = S3}
    , lists:concat([PPResp, DeployGasStr, CallGasStr])}.


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

