%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aerepl).

-export([start/0, main/1, register_includes/2, init_state/0, process_string/2,
         remove_references/3
        ]).

-include("aere_repl.hrl").


-spec default_options() -> options().
default_options() ->
    #options{ display_call_gas = false
            , display_deploy_gas = false
            , gas = 1000000
            , height = 1
            , call_value = 0
            , colors = default
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
               , letvals = []
               , letfuns = []
               , supply = 0
               }.


banner() ->
"
    ____
   / __ | ,             _     _
  / / |_|  )           | |   (_)
 ( (_____,-` ___  _ __ | |__  _  __ _
  \\______ \\ / _ \\| '_ \\| '_ \\| |/ _` |
  ,-`    ) ) (_) ) |_) ) | | | | (_| |
 (  ____/ / \\___/| .__/|_| |_|_|\\__,_|
  `(_____/       | |
                 |_|  interactive

".

main(_Args) ->
    start().

-spec start() -> finito.
start() ->
    erlang:system_flag(backtrace_depth, 100),
    io:format(banner()),
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    repl(init_state()).


-spec repl(repl_state()) -> finito.
repl(State) ->
    Inp = aere_parse:get_input(fun(Prompt) -> io:get_line(Prompt) end),
    case process_string(State, Inp) of
        finito -> finito;
        {continue, Msg, NewState} ->
            io:format("~s\n", [Msg]),
            repl(NewState)
    end.


process_string(State, String) when is_binary(String) ->
    process_string(State, binary_to_list(String));
process_string(State, String) ->
    handle_dispatch(State, aere_parse:dispatch(String)).


handle_dispatch(State, skip) ->
    {continue, "", State};
handle_dispatch(State = #repl_state{options = Opts}, {ok, {Command, Args}}) ->
    try process_input(State, Command, Args) of
        {success, Output, State1 = #repl_state{options = #options{silent = Silent}}} ->
            Msg  = case Silent of
                false -> aere_color:render_colored(Opts, aere_color:emph(aere_color:default(Output)));
                true -> ""
            end,
            {continue, Msg, State1};
        {success, State1} ->
            {continue, "", State1};
        skip ->
            {continue, "", State};
        {error, Err} ->
            Msg = aere_color:render_colored(Opts, Err),
            {continue, Msg, State};
        finito ->
            finito
    catch error:E:Stacktrace ->
            ErMsg = aere_color:render_colored(Opts,
                                              [ aere_color:red("INTERNAL ERROR:\n")
                                              , aere_error:internal(Command, E, Stacktrace)]),
            {continue, ErMsg, State};
          {error, Err} ->
            ErMsg = aere_color:render_colored(Opts, [ aere_color:red("ERROR:\n")
                                                    , Err]),
            {continue, ErMsg, State};
          X ->
            ErMsg = aere_color:render_colored(Opts, [ aere_color:red("INTERNAL ERROR:\n")
                                                    , aere_error:internal(Command, X)]),
            {continue, ErMsg, State}
    end;
handle_dispatch(State, {error, {no_such_command, "wololo"}}) ->
    put(wololo, wololo), {continue, "", State};
handle_dispatch(State = #repl_state{options = Opts}, {error, {no_such_command, Command}}) ->
    {continue, aere_color:render_colored(Opts, aere_error:no_such_command(Command)), State};
handle_dispatch(State = #repl_state{options = Opts}, {error, {ambiguous_prefix, Propositions}}) ->
    {continue, aere_color:render_colored(Opts, aere_error:ambiguous_prefix(Propositions)), State}.


ask(Question, Options, Default, REPLOpts) ->
    ValidOptions = [K || {K, _} <- Options],
    ValidOptionsStr = lists:concat([ if O =:= Default -> io_lib:format("[~p]", [O]);
                                        true -> io_lib:format("~p", [O])
                                     end
                                    || O <- ValidOptions
                                   ]),
    io:format(aere_color:render_colored(REPLOpts, aere_color:emph("~s ~s\n")), [Question, ValidOptionsStr]),
    Try = fun Retry() ->
                  Ans = string:trim(io:get_line("? ")),
                  Parsed = case Ans of
                               "" -> Default;
                               _ -> list_to_existing_atom(Ans)
                           end,
                  case proplists:get_value(Parsed, Options, {no_match}) of
                      {no_match} ->
                          io:format(
                            aere_color:render_colored(REPLOpts,
                                                      aere_color:emph(["Valid options: ", ValidOptionsStr, "\n"]))),
                          Retry();
                      Act ->
                          Act
                  end
          end,
    Try().


-define(ParseOptionBool(Field),
        if
            Val =:= "true" orelse Val =:= 1 ->
                {options, Opts#options{Field = true}};
            Val =:= "false" orelse Val =:= 0 ->
                {options, Opts#options{Field = false}};
            true ->
                aere_error:bad_option(["true", "false"])
        end).
-define(ParseOptionInt(Field),
        try {options, Opts#options{Field = list_to_integer(Val)}}
        catch error:badarg -> aere_error:bad_option(["integer"])
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
    case aere_sophia:parse_top(I) of
        {body, Body} ->
            Mock = aere_mock:chained_query_contract(State, Body),
            {NewState, Res} = eval_contract(I, Mock, State),
            {success, io_lib:format("~s", [Res]), NewState};
        [{include, _, {string, _, Inc}}] ->
            register_includes(State, [binary_to_list(Inc)]);
        [{letval, _, Pat, Expr}] -> register_letval(State, Pat, Expr);
        [FD = {fun_decl, _, _Name, _Type}] -> register_letfun(State, [FD]);
        [FD = {letfun, _, _Name, _Args, _RetType, _Body}] -> register_letfun(State, [FD]);
        [{block, _, Funs}] -> register_letfun(State, Funs);
        [TDc] when element(1, TDc) =:= type_decl ->
            repl_error:throw({unsupported_decl, type_decl});
        [TDf] when element(1, TDf) =:= type_def ->
            repl_error:throw({unsupported_decl, type_def});
        [Con] when element(1, Con) =:= contract ->
            repl_error:throw({unsupported_decl, contract});
        [Ns] when element(1, Ns) =:= namespace ->
            repl_error:throw({unsupported_decl, namespace});
        [_|_] ->
            repl_error:throw({unsupported_decl, multidecl})
    end;
process_input(State, include, Inp) ->
    Files = string:tokens(Inp, aere_parse:whitespaces()),
    register_includes(State, Files);
process_input(State = #repl_state{include_files = IFiles}, reinclude, _) ->
    register_includes(State#repl_state{ include_ast = []
                                      , include_hashes = sets:new()
                                      , include_files = []
                                      }, IFiles);
process_input(State, uninclude, _) ->
    State1 = State#repl_state{ include_ast = []
                             , include_hashes = sets:new()
                             , include_files = []
                             },
    Mock = aere_mock:simple_query_contract(State1, [{id, aere_mock:ann(), "state"}]),
    try aere_sophia:typecheck(Mock)
    catch {error, Msg} ->
            aere_error:uninclude_error(Msg)
    end,
    {success, "Unregistered all includes", State1};
process_input(State = #repl_state{options = Opts}, set, Inp) ->
    {Prop, Val0} = lists:splitwith(fun(X) -> X /= $  end, Inp),
    Val = string:trim(Val0),
    Parse =
        case Prop of
            "call_gas" -> ?ParseOptionBool(display_call_gas);
            "deploy_gas" -> ?ParseOptionBool(display_deploy_gas);
            "silent" -> ?ParseOptionBool(silent);
            "colors" -> case Val of
                            "none" -> {options, Opts#options{colors = none}};
                            "default" -> erase(wololo), {options, Opts#options{colors = default}};
                            "no_emph" -> erase(wololo), {options, Opts#options{colors = no_emph}};
                            _ -> aere_error:bad_option(["default", "none", "no_emph"])
                        end;
            "gas" -> ?ParseOptionInt(gas);
            "value" -> ?ParseOptionInt(call_value);
            "state" ->
                State1 = free_names(State, ["state", "put"]),
                Stmts = aere_sophia:parse_body(Val),
                TExprAst = aere_sophia:typecheck(aere_mock:simple_query_contract(State1, Stmts)),
                {_, Type} = aere_sophia:type_of(TExprAst, ?USER_INPUT),
                Mock = aere_mock:chained_initial_contract(State1, Stmts, Type),
                TMock = aere_sophia:typecheck(Mock),
                S0 = State#repl_state.chain_state,
                {{Key, _}, S1} = build_deploy_contract("no_src", TMock, {}, Opts, S0),
                {state, State1#repl_state
                 { user_contract_state_type = Type
                 , user_contracts = [Key]
                 , chain_state = S1
                 }};
            _ -> aere_error:unknown_option(Prop)
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
process_input(State = #repl_state{ tracked_contracts = Cons
                                 , letvals = Letvals
                                 , chain_state = S0
                                 , options = Opts
                                 }, deploy, Inp) ->
    {File, MaybeRefName} =
        case string:tokens(Inp, aere_parse:whitespaces()) of
            [] -> aere_error:no_file_deploy();
            [F] -> {F, none};
            [F, "as", N] ->
                Valid = fun(C) -> ((C >= $a) and (C =< $z))
                                      or ((C >= $A) and (C =< $Z))
                                      or ((C >= $0) and (C =< $9))
                                      or (C == $_)
                        end,
                [aere_error:bad_deploy_name()
                 || (lists:nth(1, N) < $a) or (lists:nth(1, N) > $z)],
                [aere_error:bad_deploy_name()
                 || not lists:all(Valid, N)],
                {F, N};
            _ -> aere_error:parse_deploy()
        end,
    case file:read_file(File) of
        {ok, Src} ->
            Ast = aere_sophia:parse_file(binary_to_list(Src), []),
            TAst = aere_sophia:typecheck(Ast),
            BCode = aere_sophia:compile_contract(fate, binary_to_list(Src), TAst),
            {{con, DecAnn, StrDeclName}, Interface}
                = aere_sophia:generate_interface_decl(TAst),
            RefName =
                case MaybeRefName of
                    none ->
                        [FstChar|OrigNameRest] = StrDeclName,
                        NameWithIndex =
                            fun(X) -> string:lowercase([FstChar]) ++
                                          OrigNameRest ++ case X of
                                                              -1 -> "";
                                                              _ -> integer_to_list(X)
                                                          end
                            end,
                        MakeIndex =
                            fun R(X) ->
                                    case name_status(State, NameWithIndex(X)) of
                                        free -> X;
                                        _    -> R(X + 1)
                                    end
                            end,
                        NameWithIndex(MakeIndex(-1));
                    _ -> MaybeRefName
                end,
            {State1, Sup} = next_sup(State), %% NOTE supply changes in parallel to chain_state
            ConDeclName1 =
                    {con, DecAnn, ?TrackedContractName(RefName, StrDeclName) ++ io_lib:format("~p", [Sup])},
            Interface1 =
                begin
                    {contract, IAnn, _, Body} = Interface,
                    {contract, IAnn, ConDeclName1, Body}
                end,

            {{Con, DeployGas}, S1} = deploy_contract(BCode, {}, Opts, S0),
            DepGasStr = case Opts#options.display_deploy_gas of
                            true -> [aere_color:yellow("\ndeploy gas"), ": ", DeployGas];
                            false -> ""
                        end,
            {Cons1, Letvals1} = remove_references([RefName], Cons, Letvals),
            NewState = State1#repl_state
                { chain_state = S1
                , tracked_contracts = [{RefName, {tracked_contract, Con, ConDeclName1, Interface1}} | Cons1]
                , letvals = Letvals1
                },
            { success, [ aere_color:green(RefName ++ " : " ++ StrDeclName)
                       , " was successfully deployed"
                       , DepGasStr
                       ]
            , NewState};
        {error, Reason} ->aere_error:file_error(File, Reason)
    end;
process_input(State, rm, Inp) ->
    {success, free_names(State, string:split(Inp, aere_parse:whitespaces()))};
process_input(S, pwd, _) ->
    shell_default:pwd(),
    {success, S};
process_input(S, cd, Inp) ->
    shell_default:cd(Inp),
    {success, S};
process_input(S, ls, _) ->
    shell_default:ls(),
    {success, S};
%% process_input(S, list, Inp) ->
%%     Out =
%%         case Inp of
%%             "contracts" -> io_lib:format("~p", [[N || {N, {tracked_contract, _, _, _}} <- S#repl_state.tracked_contracts]]);
%%             "vals"      -> io_lib:format("~p", [[N || {{N, _}, _} <- S#repl_state.letvals]]);
%%             "functions" -> io_lib:format("~p", [[N || {N, _} <- S#repl_state.letfuns]]);
%%             _ -> throw({error, "I don't understand. I can print you list of: contracts, let, def, letval, letfun, names"})
%%         end,
%%     {success, Out, S};
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
    aere_error:undefined_command().

register_letval(S0 = #repl_state{ letvals = Letvals
                                , tracked_contracts = Cons
                                , options = Opts
                                , chain_state = CS0
                                }, Pat, Expr) ->
    {S1, PName} = make_provider_name(S0, Pat),
    Provider = aere_mock:letval_provider(S0, PName, Expr),
    TProvider = aere_sophia:typecheck(Provider),
    {[], Type} = aere_sophia:type_of(TProvider, ?LETVAL_GETTER(PName)),
    {{Ref, _}, CS1} = build_deploy_contract("no_source", TProvider, {}, Opts, CS0),
    {Cons1, Letvals1} = remove_references(aere_sophia:get_pat_ids(Pat) ,Cons, Letvals),
    S2 = S1#repl_state{ letvals = [{{PName, Ref}, {Pat, Type}}|Letvals1]
                      , tracked_contracts = Cons1
                      , chain_state = CS1
                      },
    {success, S2}.

register_letfun(S, []) ->
    {success, S};
register_letfun(S0 = #repl_state{letfuns = Letfuns, letvals = Letvals, tracked_contracts = Cons}, Funs) ->
    Name = case Funs of
               [{fun_decl, _, {id, _, N}, _}|_] -> N;
               [{letfun, _, {id, _, N}, _, _, _}|_] -> N
           end,
    case Name of
        "init" -> aere_error:forbidden_id(Name);
        _ -> ok
    end,
    {Cons1, Letvals1} = remove_references([Name], Cons, Letvals),
    {S1, Shadowed} =
        case proplists:get_value(Name, Letfuns, none) of
            none -> {S0, Letfuns};
            Dupl ->
                begin
                    {SS, NewName} = make_shadowed_fun_name(S0, Name),
                    UpdateName =
                        fun(NN) ->
                                case NN == Name of
                                    true -> NewName;
                                    false -> NN
                                end end,
                    {SS, [{FName, {[case F of
                                        {fundecl, A, {id, AName, Fn}, RT} ->
                                            {fundecl, A, {id, AName, UpdateName(Fn)}, RT};
                                        {letfun, A, {id, AName, Fn}, Args, RT, B} ->
                                            {letfun, A, {id, AName, UpdateName(Fn)}, Args, RT,
                                             begin
                                             aere_sophia:replace_var(B, Name, NewName)
                                             end}
                                    end
                                    || F <- Fs], Cs, Ls}}
                          || {FName, {Fs, Cs, Ls}} <- [{NewName, Dupl}|proplists:delete(Name, Letfuns)]
                         ]}
                end
        end,
    S2 = S1#repl_state{ letfuns = [{Name, {Funs, Cons1, Letvals1}} | Shadowed]
                      , letvals = Letvals1
                      , tracked_contracts = Cons1
                      },
    TestMock = aere_mock:chained_query_contract(S2, [{id, aere_mock:ann(), "state"}]),
    aere_sophia:typecheck(TestMock),
    {success, S2}.

make_provider_name(S0, Pat) ->
    Ids = aere_sophia:get_pat_ids(Pat),
    {S1, Sup} = next_sup(S0),
    {S1, string:join(Ids, "_") ++ io_lib:format("#~p", [Sup])}.

make_shadowed_fun_name(S0, Name) ->
    {S1, Sup} = next_sup(S0),
    {S1, Name ++ io_lib:format("#~p", [Sup])}.

remove_references(Names, Cons, LetVals) ->
    { [ case lists:member(CName, Names) of
            true -> {CName, {shadowed_contract, ConRef, ConName, I}};
            false -> C
        end
        || C = {CName, {_, ConRef, ConName, I}} <- Cons
      ]
    , [ {{Provider, ProvRef}, {NewPat, Type}}   %% removing letvals shadowed by rec and args
        || {{Provider, ProvRef}, {Pat, Type}} <- LetVals,
           NewPat <- [lists:foldl(
                        fun(V, P) -> aere_sophia:replace_var(P, V, "_")
                        end, Pat, Names)],
           lists:any(fun({id, _, "_"}) -> false; %% If everything is removed, why even consider it?
                        (_) -> true
                     end, aere_sophia:get_pat_ids(NewPat))
      ]
    }.

free_names(State = #repl_state{letfuns = Letfuns, letvals = Letvals, tracked_contracts = Cons, options = Opts}, Names) ->
    RunCleansing =
        fun Cleansing([], [], L) ->
                L;
            Cleansing(_, _, []) ->
                [];
            Cleansing([], NextWave, L) ->
                Cleansing(NextWave, [], L);
            Cleansing([Bad|Rest], NextWave, Fs) ->
                {ToRemove, ToKeep} =
                    lists:partition
                      (fun({_, {Defs, _, _}}) ->
                               lists:any(
                                 fun(Def) ->
                                         UsedNames = aeso_syntax_utils:used_ids(Def),
                                         lists:member(Bad, UsedNames)
                                 end, Defs)
                       end, Fs),
                Cleansing(Rest, [NewBad || {NewBad, _} <- ToRemove] ++ NextWave, ToKeep)
        end,
    Letfuns1 = [ F
                || F = {Fn, _} <- RunCleansing(Names, [], Letfuns),
                   not(lists:member(Fn, Names))
               ],
    {Cons1, Letvals1} = remove_references(Names, Cons, Letvals),
    State1 = State#repl_state
        { letfuns = Letfuns1
        , letvals = Letvals1
        , tracked_contracts = Cons1
        },
    case ([FN || {FN, _} <- Letfuns -- Letfuns1, not(lists:member(FN, Names))]) of
        [] -> State1;
        AdditionalNames ->
            ask(io_lib:format("This will require removing following entities: ~s. Proceed?",
                              [string:join(AdditionalNames, ", ")]
                             ), [{y, State1}, {n, State}], y, Opts)
    end.


name_status(#repl_state
            { letvals = LetDefs
            , letfuns = LocFuns
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

            MockForTc = aere_mock:simple_query_contract(NewState, [{id, aere_mock:ann(), "state"}]),
            aere_sophia:typecheck(MockForTc),

            Colored = aere_color:yellow(lists:flatten([" " ++ F || F <- Files])),
            IncludeWord = case Files of
                              []  -> "nothing";
                              [_] -> "include";
                              _   -> "includes"
                          end,
            {success, ["Registered ", IncludeWord, Colored], NewState};
        Duplicates ->
            Colored = aere_color:yellow(lists:flatten([" " ++ D || D <- Duplicates])),
            {error, ["Following files are already included: ", Colored]}
    end.


build_deploy_contract(Src, TypedAst, Args, Options, S0) ->
    Code = aere_sophia:compile_contract(fate, Src, TypedAst),
    deploy_contract(Code, Args, Options, S0).


deploy_contract(ByteCode, Args, Options, S0) ->
    aere_chain:state(S0),
    Serialized = aect_sophia:serialize(ByteCode, aere_version:contract_version()),
    {Owner, S1} = aere_chain:new_account(100000021370000999, S0),
    try aere_chain:create_contract(Owner, Serialized, Args, Options, S1)
    catch error:{failed_contract_create, Reason} ->
            ReasonS = if is_binary(Reason) -> binary_to_list(Reason);
                         is_list(Reason) -> Reason;
                         true -> io_lib:format("~p", [Reason])
                      end,
            aere_error:contract_creation_error(ReasonS)
    end.


eval_contract(Src, Ast, State = #repl_state{options = Options}) ->
    TypedAst = aere_sophia:typecheck(Ast),
    RetType = aere_response:convert_type( build_type_map(TypedAst)
                                        , element(2, aere_sophia:type_of(TypedAst, ?USER_INPUT))),
    S0 = State#repl_state.chain_state,
    {{Con, GasDeploy}, S1} = build_deploy_contract(Src, TypedAst, {}, Options, S0),
    {Owner, S2} = aere_chain:new_account(100000021370000999, S1),
    {{Resp, GasCall}, S3} =
        aere_chain:call_contract( Owner, Con, list_to_binary(?USER_INPUT)
                                  , RetType, {}, Options, S2),

    PPResp = prettypr:format(aere_response:pp_response(Resp)),
    DeployGasStr =
        case Options#options.display_deploy_gas of
            true -> [aere_color:yellow("\ndeploy gas"), ": ", GasDeploy];
            false -> ""
        end,
    CallGasStr =
        case Options#options.display_call_gas of
            true -> [aere_color:yellow("\ncall gas"), ": ", GasCall];
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

next_sup(State = #repl_state{supply=S}) ->
    {State#repl_state{supply=S+1}, S}.
