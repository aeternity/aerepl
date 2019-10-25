%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aerepl).

-export([start/0, main/1, register_includes/2, init_state/0, process_string/2]).

-include("aere_repl.hrl").


-spec default_options() -> options().
default_options() ->
    #options{ display_call_gas = false
            , display_deploy_gas = false
            , gas = 1000000
            , height = 1
            , call_value = 0
            , backend = fate
            , silent = false
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
               , tracked_contracts = []
               , let_defs = []
               , local_funs = []
               }.


logo() ->
"
   ____
  / __ | ,             _     _
 / / |_|  )           | |   (_)
( (_____,-` ___  _ __ | |__  _  __ _
 \\______ \\ / _ \\| '_ \\| '_ \\| |/ _` |
 .-`    ) ) (_) | |_) | | | | | (_| |
(  ____/ / \\___/| .__/|_| |_|_|\\__,_|
 `(_____/       | |
                |_|  interactive

".

main(_Args) ->
    start().

-spec start() -> finito.
start() ->
    erlang:system_flag(backtrace_depth, 100),
    io:format(logo()),
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    repl(init_state()).


-spec repl(repl_state()) -> finito.
repl(State) ->
    Inp = aere_parse:get_input(fun(Prompt) -> io:get_line(Prompt) end),
    case process_string(State, Inp) of
        finito -> finito;
        {continue, Msg, NewState} ->
            io:format(Msg),
            repl(NewState)
    end.


process_string(State, String) when is_binary(String) ->
    process_string(State, binary_to_list(String));
process_string(State, String) ->
    handle_dispatch(State, aere_parse:dispatch(String)).


handle_dispatch(State, skip) ->
    {continue, "", State};
handle_dispatch(State, {ok, {Command, Args}}) ->
    try process_input(State, Command, Args) of
        {success, Output, State1 = #repl_state{options = #options{silent = Silent}}} ->
            Msg  = case Silent of
                false -> io_lib:format("~s\n", [aere_color:emph(Output)]);
                true -> ""
            end,
            {continue, Msg, State1};
        {success, State1} ->
            {continue, "", State1};
        skip ->
            {continue, "", State};
        {error, Err} ->
            Msg = io_lib:format("~s:~n~s~n", [aere_color:red("Error"), aere_color:emph(Err)]),
            {continue, Msg, State};
        finito ->
            finito
    catch error:E ->
            CommandStr = aere_color:blue(lists:flatten(io_lib:format("~p", [Command]))),
            ErStr = aere_color:red(lists:flatten(io_lib:format("~p", [E]))),
            ErMsg = io_lib:format("Command ~s failed:\n~s\n", [CommandStr, ErStr])
                ++ io_lib:format("Stacktrace:\n" ++ aere_color:emph("~p") ++"\n\n", [erlang:get_stacktrace()])
                ++ aere_color:red("*** This is an internal error and most likely a bug.\n"),
            {continue, ErMsg, State};
          {error, Err} ->
            ErMsg = io_lib:format("~s:~n~s~n", [aere_color:red("Error"), aere_color:emph(Err)]),
            {continue, ErMsg, State};
          X ->
            CommandStr = lists:flatten(aere_color:blue(io_lib:format("~p", [Command]))),
            ErMsg = io_lib:format("Command ~s failed with\n~p\n", [CommandStr, X])
                ++ aere_color:red("*** This is an internal error and most likely a bug.\n"),
            {continue, ErMsg, State}
    end;
handle_dispatch(State, {error, {no_such_command, "wololo"}}) ->
    put(wololo, wololo), {continue, "", State};
handle_dispatch(State, {error, {no_such_command, Command}}) ->
    Msg = io_lib:format("No such command " ++ aere_color:blue("~p") ++ "\n", [Command]),
    {continue, Msg, State};
handle_dispatch(State, {error, {ambiguous_prefix, Propositions}}) ->
    PropsString =
        aere_color:blue(lists:flatten([io_lib:format(" ~p", [P]) || P <- Propositions])),
    Msg = io_lib:format("Ambiguous command prefix. Matched commands: ~s\n", [PropsString]),
    {continue, Msg, State}.


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
    register_includes(State#repl_state{ include_ast = []
                                      , include_hashes = sets:new()
                                      , include_files = []
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
            "silent" -> ?ParseOptionBool(silent);
            "gas" -> ?ParseOptionInt(gas);
            "value" -> ?ParseOptionInt(call_value);
            "aevm" -> { options, "Not supported at all, use at your own responsibility"
                      , Opts#options{backend = aevm}};
            "fate" -> {options, Opts#options{backend = fate}};
            "state" ->
                State1 = State#repl_state{local_funs = []},
                Expr = aere_sophia:parse_body(Val),
                TExprAst = aere_sophia:typecheck(aere_mock:simple_query_contract(State1, Expr)),
                {_, Type} = aere_sophia:type_of(TExprAst, ?USER_INPUT),
                Mock = aere_mock:chained_initial_contract(State1, Expr, Type),
                TMock = aere_sophia:typecheck(Mock),
                S0 = State#repl_state.chain_state,
                {{Key, _}, S1} = build_deploy_contract("no_src", TMock, {}, Opts, S0),
                {state, State1#repl_state
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
    {File, MaybeRefName} =
        case string:tokens(Inp, aere_parse:whitespaces()) of
            [] -> throw({error, "What to deploy? Give me some file"});
            [F] -> {F, none};
            [F, "as", N] ->
                Valid = fun(C) -> ((C >= $a) and (C =< $z))
                                      or ((C >= $A) and (C =< $Z))
                                      or ((C >= $0) and (C =< $9))
                                      or (C == $_)
                        end,
                [throw({error, "Name must begin with a lowercase letter"})
                 || (lists:nth(1, N) < $a) or (lists:nth(1, N) > $z)],
                [throw({error, "Name must consist of letters, numbers or underscore"})
                 || not lists:all(Valid, N)],
                {F, N};
            _ -> throw({error, "Bad input format"})
        end,
    case file:read_file(File) of
        {ok, Src} ->
            Ast = aere_sophia:parse_file(binary_to_list(Src), []),
            TAst = aere_sophia:typecheck(Ast),
            BCode = aere_sophia:compile_contract(Opts#options.backend, binary_to_list(Src), TAst),
            {{con, DecAnn, StrDeclName}, Interface}
                = aere_sophia:generate_interface_decl(TAst),
            RefName =
                case MaybeRefName of
                    none ->
                        [FstChar|OrigNameRest] = StrDeclName,
                        NameWithIndex =
                            fun(X) -> string:lowercase([FstChar]) ++
                                          OrigNameRest ++ integer_to_list(X) end,
                        MakeIndex =
                            fun R(X) ->
                                    case name_status(State, NameWithIndex(X)) of
                                        free -> X;
                                        _    -> R(X + 1)
                                    end
                            end,
                        NameWithIndex(MakeIndex(0));
                    _ ->
                        check_name_conflicts(State, MaybeRefName, [letfun_local]),
                        MaybeRefName
                end,
            ConDeclName1 =
                    {con, DecAnn, ?TrackedContractName(RefName, StrDeclName)},
            Interface1 =
                begin
                    {contract, IAnn, _, Body} = Interface,
                    {contract, IAnn, ConDeclName1, Body}
                end,

            {{Con, DeployGas}, S1} = deploy_contract(BCode, {}, Opts, S0),
            DepGasStr = case Opts#options.display_deploy_gas of
                            true -> lists:concat([aere_color:yellow("\ndeploy gas"), ": ", DeployGas]);
                            false -> ""
                        end,
            NewState = State#repl_state
                { chain_state = S1
                , tracked_contracts =
                      [{RefName, {tracked_contract, Con, ConDeclName1, Interface1}}
                       | proplists:delete(RefName, Contracts)]
                },
            { success, lists:concat([ aere_color:green(RefName ++ " : " ++ StrDeclName)
                                    , aere_color:emph(" was successfully deployed")
                                    , DepGasStr
                                    ])
            , NewState};
        {error, _} -> throw({error, "Could not load file " ++ aere_color:yellow(File)})
    end;
process_input(State = #repl_state{ options = Opts
                                 , chain_state = S0
                                 }, 'let', Inp) ->
    case aere_sophia:parse_letdef(Inp) of
        {letval, _, {id, _, Name}, _, Body} ->
            check_name_conflicts(State, Name, [letfun_local]),
            Provider = aere_mock:letdef_provider(State, Name, Body),
            TProvider = aere_sophia:typecheck(Provider),
            {[], Type} = aere_sophia:type_of(TProvider, ?LETVAL_GETTER(Name)),
            {{Ref, _}, S1} = build_deploy_contract(Inp, TProvider, {}, Opts, S0),
            NewState = register_letdef( State#repl_state{ chain_state = S1}
                                      , Name, {letval, Ref, Type});
        {letfun, _, {id, _, Name}, Args, _, Body} ->
            check_name_conflicts(State, Name, [letfun_local]),
            State1 = State#repl_state{
                       let_defs = [L || L = {N, Def} <- State#repl_state.let_defs,
                                         element(1, Def) =:= letval orelse N =/= Name
                                  ]},
            Provider = aere_mock:letdef_provider(State1, Name, Args, Body),
            TProvider = aere_sophia:typecheck(Provider),
            {ArgsT, RetT} = aere_sophia:type_of(TProvider, Name),
            {{Ref, _}, S1} = build_deploy_contract(Inp, TProvider, {}, Opts, S0),
            NewState = register_letdef( State1#repl_state{ chain_state = S1}
                                      , Name, {letfun, Ref, Args, {ArgsT, RetT}})
    end,
    {success, NewState};
process_input(State = #repl_state{ local_funs = LocFuns
                                 , let_defs = LetDefs
                                 }, def, Inp) ->
    case aere_sophia:parse_letdef(Inp) of
        {letfun, _, {id, _, Name}, Args, _, Body} ->
            check_name_conflicts(State, Name, [letfun_local, letfun]),
            LetDefs1 = proplists:delete(Name, LetDefs),  % remove letval
            State1 = State#repl_state
                { local_funs = [{Name, {letfun_local, Args, Body, LetDefs1}} | LocFuns]
                , let_defs = LetDefs1
                },
            TestMock = aere_mock:chained_query_contract
                         ( State1#repl_state{let_defs = [L || L = {letval, _, _} <- LetDefs1]}
                        , {id, aere_mock:ann(), "state"}),
            aere_sophia:typecheck(TestMock),
            {success, State1};
        {letval, _, _, _, _} -> throw({error, "Use :let to define constants"})
    end;
process_input(State, unlet, Inp) ->
    check_name_conflicts(State, Inp, [free]),
    {success, unregister_letdef(State, Inp)};
process_input(State, undef, "") ->
    {success, State#repl_state{local_funs = []}};
process_input(_, undef, _) ->
    throw({error, "Arguments are not expected here"});
process_input(State, undeploy, Inp) ->
    check_name_conflicts(State, Inp, [free]),
    {success, unregister_contract(State, Inp)};
process_input(State, rm, Inp) ->
    {success, free_name(State, Inp)};
process_input(S, pwd, _) ->
    shell_default:pwd(),
    {success, S};
process_input(S, cd, Inp) ->
    shell_default:cd(Inp),
    {success, S};
process_input(S, list, Inp) ->
    Out = case Inp of
              "contracts" -> io_lib:format("~p", [[N || {N, _} <- S#repl_state.tracked_contracts]]);
              "let" -> io_lib:format("~p", [[N || {N, _} <- S#repl_state.let_defs]]);
              "def" -> io_lib:format("~p", [[N || {N, _} <- S#repl_state.local_funs]]);
              "letval" -> io_lib:format("~p", [[N || {N, L} <- S#repl_state.let_defs, element(1, L) =:= letval]]);
              "letfun" -> io_lib:format("~p", [[N || {N, L} <- S#repl_state.let_defs, element(1, L) =:= letfun]]);
              "names" -> io_lib:format("~p", [[N || {N, _} <- S#repl_state.tracked_contracts ++
                                                        S#repl_state.let_defs ++ S#repl_state.local_funs]]);
              _ -> throw({error, "I don't understand. I can print you list of: contracts, let, def, letval, letfun, names"})
          end,
    {success, Out, S};
process_input(S, load, Inp) ->
    Agg = lists:foldl(fun(Command, Prev) ->
                            case Prev of
                                {continue, Msgs, PrevS} ->
                                    case handle_dispatch(PrevS, Command) of
                                        {continue, Msg, NewS} ->
                                            {continue, Msgs ++ Msg, NewS};
                                        finito -> finito
                                    end;
                                finito -> finito
                            end
                    end, {continue, "", S}, aere_parse:eval_from_file(Inp)),
    case Agg of
        {continue, M, NS} ->
            {success, string:trim(M), NS};
        finito -> finito
    end;
process_input(_, _, _) ->
    throw({error, "This command is not defined yet (but should be)."}).


unregister_letdef(State = #repl_state{let_defs = LetDefs}, Name) ->
    State#repl_state{let_defs = proplists:delete(Name, LetDefs)}.


register_letdef(State = #repl_state{let_defs = LetDefs}, Name, Def) ->
    State#repl_state{let_defs = proplists:delete(Name, LetDefs) ++ [{Name, Def}]}.


unregister_contract(State = #repl_state{tracked_contracts = Cons}, Name) ->
    State#repl_state{tracked_contracts = proplists:delete(Name, Cons)}.


free_name(State, Name) ->
    check_name_conflicts(State, Name, [free, letfun_local]),
    unregister_contract(unregister_letdef(State, Name), Name).


name_status(#repl_state
            { let_defs = LetDefs
            , local_funs = LocFuns
            , tracked_contracts = TCons
            }, Name) ->
    % Beware, the state-of-the-art solution is approaching!
    case { proplists:is_defined(Name, LetDefs)
         , proplists:is_defined(Name, LocFuns)
         , proplists:is_defined(Name, TCons)
         } of
        {true, _, _} ->
            case proplists:lookup(Name, LetDefs) of
                {_, {letval, _, _}}    -> letval;
                {_, {letfun, _, _, _}} -> letfun
            end;
        {_, true, _} -> letfun_local;
        {_, _, true} -> tracked_contract;
        _            -> free
    end.


check_name_conflicts(State, Name, Conflicts) ->
    Status = name_status(State, Name),
    case lists:member(Status, Conflicts) of
        true ->
            case Status of
                letfun_local ->
                    throw({error,
                           "This name already belongs to a def-function\n"
                           "Use different name or clear def-function index by typing :undef"
                          });
                letfun ->
                    throw({error,
                           "This name already belongs to a let-function\n"
                           "Use different name or free it by typing :unlet " ++ Name
                          });
                letval ->
                    throw({error,
                           "This name already belongs to a let-value\n"
                           "Use different name or free it by typing :unlet " ++ Name
                          });
                tracked_contract ->
                    throw({error,
                           "This name already belongs to a tracked contract\n"
                           "Use different name or free it by typing :undeploy " ++ Name
                          });
                free ->
                    throw({error,
                           "This name is not taken by any repl object\n"
                          });
                _ -> error("Unknown status?")
            end;
        false ->
            ok
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
            {Addition, NewHashes} = aere_sophia:parse_file(IncludingContract, Hashes, [keep_included]),
            NewIncludes = Includes ++ Addition,
            NewState = State#repl_state{ include_ast    = NewIncludes
                                       , include_hashes = NewHashes
                                       , include_files  = Files ++ PrevFiles
                                       },
            aere_sophia:typecheck(
                  aere_mock:simple_query_contract(NewState, {id, [], "state"})),
            Colored = aere_color:yellow(lists:flatten([" " ++ F || F <- Files])),
            IncludeWord = case Files of
                              []  -> "nothing";
                              [_] -> "include";
                              _   -> "includes"
                          end,
            {success, "Registered " ++ IncludeWord ++ Colored, NewState};
        Duplicates ->
            Colored = aere_color:yellow(lists:flatten([" " ++ D || D <- Duplicates])),
            {error, io_lib:format("Following files are already included: ~s", [Colored])}
    end.


build_deploy_contract(Src, TypedAst, Args, Options = #options{backend = Backend}, S0) ->
    Code = aere_sophia:compile_contract(Backend, Src, TypedAst),
    deploy_contract(Code, Args, Options, S0).


deploy_contract(ByteCode, Args, Options, S0) ->
    aere_runtime:state(S0),
    Serialized = aect_sophia:serialize(ByteCode, aere_version:contract_version()),
    {Owner, S1} = aere_runtime:new_account(100000021370000999, S0),
    try aere_runtime:create_contract(Owner, Serialized, Args, Options, S1)
    catch error:{failed_contract_create, Reason} ->
            ReasonS = if is_binary(Reason) -> binary_to_list(Reason);
                         is_list(Reason) -> Reason;
                         true -> io_lib:format("~p", [Reason])
                      end,
            throw({error, "Failed to create contract: " ++ ReasonS})
    end.


eval_contract(Src, Ast, State = #repl_state{options = Options}) ->
    %% io:format("~p\n\n", [Ast]), %% TODO: REMOVE IT
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

