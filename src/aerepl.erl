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
            , display_unit = false
            }.


-spec init_state() -> repl_state().
init_state() ->
    {PK, ChainState} = aere_chain:new_account(100000000000000000000000000000, #{}),
    #repl_state{ include_ast = []
               , include_hashes = sets:new()
               , include_files = []
               , options = default_options()
               , chain_state = ChainState
               , user_contract_state_type = {tuple_t, [], []}
               , user_contracts = []
               , tracked_contracts = []
               , letvals = []
               , letfuns = []
               , typedefs = []
               , type_alias_map = []
               , user_account = PK
               , warnings = []
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


%% load_nifs() ->
%%     case os:getenv("NIF_DIR") of
%%         false -> Path = "_build/default/lib/";
%%         Path -> Path
%%     end,
%%     ok = enacl_nif:load(Path).

main(_Args) ->
    %% load_nifs(),
    start().

-spec start() -> finito.
start() ->
    erlang:system_flag(backtrace_depth, 100),
    io:format(banner()),
    application:set_env(aecore, network_id, <<"local_lima_testnet">>),
    loop(init_state()).


destroy_warnings(State = #repl_state{warnings = Ws}) ->
    {State#repl_state{warnings = []}, Ws}.

print_msg(#repl_state{options = Opts}, Msg) ->
    print_msg(Opts, Msg);
print_msg(O = #options{}, Msg) ->
    Render = lists:flatten(aere_color:render_colored(O, Msg)),
    io:format("~s", [string:trim(Render, both, aere_parse:whitespaces()) ++ "\n"]).


-spec loop(repl_state()) -> finito.
loop(State) ->
    Inp = aere_parse:get_input(fun(Prompt) -> io:get_line(Prompt) end),
    case process_string(State, Inp) of
        Resp = #repl_response{} ->
            case Resp#repl_response.status of
                {success, State1} ->
                    case Resp#repl_response.warnings of
                        [] -> ok;
                        Warnings ->
                            [print_msg(State1, [aere_color:yellow("Warning: "), W, "\n"]) || W <- Warnings]
                    end,
                    print_msg(State1, Resp#repl_response.output),
                    loop(State1);
                ask -> todo;
                error ->
                    print_msg(State, Resp#repl_response.output),
                    loop(State);
                internal_error ->
                    print_msg(State, Resp#repl_response.output),
                    loop(State);
                finito -> finito
            end;
        Q = #repl_question{} ->
            QR = fun Retry(MyQ = #repl_question{text = Text}) ->
                         print_msg(State, [Text, " ", valid_question_options_str(MyQ)]),
                         Ans = string:trim(io:get_line("? "), both, aere_parse:whitespaces()),
                         case answer(MyQ, Ans) of
                             {retry, MyQ1} -> Retry(MyQ1);
                             {accept, X} -> X
                         end
                 end,
            loop(QR(Q))
    end.

valid_question_options_str(#repl_question
                           { options = Options
                           , default = Default
                           }) ->
    ValidOptions = [K || {K, _} <- Options],
    lists:concat([ if O =:= Default -> io_lib:format("[~s]", [O]);
                      true -> io_lib:format("~s", [O])
                   end
                   || O <- ValidOptions
                 ]).

answer(Q = #repl_question
    { options = Options
    , default = Default
    , callback = Callback
    }, Ans) ->
    Parsed =
        case Ans of
            "" -> Default;
            _ -> Ans
        end,
    case proplists:get_value(Parsed, Options, {no_match}) of
        {no_match} ->
            Notification = aere_color:emph("Invalid option."),
            {retry, Q#repl_question{text = Notification}};
        Act ->
            {accept, Callback(Act())}
    end.


-spec process_string(repl_state(), binary() | string())
                    -> repl_response() | repl_question() | finito.
process_string(State, String) when is_binary(String) ->
    process_string(State, binary_to_list(String));
process_string(State, String) ->
    Proc = aere_parse:dispatch(String),
    handle_dispatch(State, Proc).


handle_dispatch(State, skip) ->
    #repl_response
        { output = ""
        , warnings = []
        , status = {success, State}
        };
handle_dispatch(State = #repl_state{}, {ok, {Command, Args}}) ->
    try process_input(State, Command, Args) of
        {Output, State1 = #repl_state{options = #options{silent = Silent}}} ->
            {State2, Warnings} = destroy_warnings(State1),
            Msg = ?IF(Silent, "", aere_color:emph(aere_color:default(Output))),
            #repl_response
                { output = Msg
                , warnings = Warnings
                , status = {success, State2}
                };
        State1 = #repl_state{} ->
            #repl_response
                { output = ""
                , warnings = []
                , status = {success, State1}
                };
        Question = #repl_question{} ->
            Question;
        skip ->
            #repl_response
                { output = ""
                , warnings = []
                , status = {success, State}
                };
        finito ->
            #repl_response
                { output = ""
                , warnings = []
                , status = finito
                };
        WTF ->
            Msg = aere_color:emph(
                    aere_color:red(
                      io_lib:format("Unexpected dispatch: ~p", [WTF]))),
            #repl_response
                { output = Msg
                , warnings = []
                , status = internal_error
                }
    catch error:E:Stacktrace ->
            Msg = aere_error:internal(Command, E, Stacktrace),
            #repl_response
                { output = Msg
                , warnings = []
                , status = internal_error
                };
          {error, Msg} ->
            #repl_response
                { output = Msg
                , warnings = []
                , status = error
                }
    end;
handle_dispatch(State, {error, {no_such_command, "wololo"}}) ->
    put(wololo, wololo),
    #repl_response
        { output = ""
        , warnings = []
        , status = {success, State}
        };
handle_dispatch(#repl_state{}, {error, {no_such_command, Command}}) ->
    Msg = aere_error:no_such_command(Command),
    #repl_response
        { output = Msg
        , warnings = []
        , status = error
        };
handle_dispatch(#repl_state{}, {error, {ambiguous_prefix, Propositions}}) ->
    Msg = aere_error:ambiguous_prefix(Propositions),
    #repl_response
        { output = Msg
        , warnings = []
        , status = error
        }.


-spec process_input(repl_state(), aere_parse:command(), string()) ->
          finito | {error, string()} | {success, string(), repl_state()}
                 | {success, repl_state()}.
process_input(_, quit, _) ->
    finito;
process_input(_, reset, _) ->
    init_state();
process_input(State, type, I) ->
    Stmts = aere_sophia:parse_body(I),
    Contract = aere_mock:chained_query_contract(State, unfold_aliases(State, Stmts)),
    TAst = aere_sophia:typecheck(Contract, [dont_unfold]),
    {_, Type} = aere_sophia:type_of(TAst, ?USER_INPUT),
    {aeso_ast_infer_types:pp_type("", Type), State};
process_input(State, eval, I) ->
    Parse = aere_sophia:parse_top(I),
    case Parse of
        {body, Body} ->
            Mock = aere_mock:chained_query_contract(State, unfold_aliases(State, Body)),
            {NewState, Res} = eval_contract(I, Mock, State),
            { case Res of
                  "()" -> ?IF(State#repl_state.options#options.display_unit, "()", "");
                  _ -> io_lib:format("~s", [Res])
              end
            , NewState};
        [{include, _, {string, _, Inc}}] ->
            register_includes(State, [binary_to_list(Inc)]);
        [{letval, _, Pat, Expr}] -> register_letval(State, Pat, Expr);
        [FD = {fun_decl, _, _Name, _Type}] -> register_letfun(State, [FD]);
        [FD = {letfun, _, _Name, _Args, _RetType, _Body}] -> register_letfun(State, [FD]);
        [{block, _, Funs}] -> register_letfun(State, Funs);
        [TDf] when element(1, TDf) =:= type_def ->
            register_typedef(State, TDf);

        [TDc] when element(1, TDc) =:= type_decl ->
            repl_error:unsupported_decl(type_decl);
        [Con] when element(1, Con) =:= contract ->
            repl_error:unsupported_decl(contract);
        [Ns] when element(1, Ns) =:= namespace ->
            repl_error:unsupported_decl(namespace);
        [_|_] ->
            repl_error:unsupported_decl(multidecl)
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
    try assert_integrity(State1)
    catch {error, Msg} ->
            aere_error:uninclude_error(Msg)
    end,
    {"Unregistered all includes", State1};
process_input(State, set, Inp) ->
    {Prop, Val0} = lists:splitwith(fun(X) -> X /= $  end, Inp),
    Val = string:trim(Val0),
    set_option(State, Prop, Val);
process_input(State, deploy, Inp) ->
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
    Src = case file:read_file(File) of
              {ok, Src_} -> Src_;
              {error, Reason} -> aere_error:file_error(File, Reason)
          end,
    register_tracked_contract(State, MaybeRefName, Src);
process_input(State, rm, Inp) ->
    State1 = free_names(State, string:split(Inp, aere_parse:whitespaces())),
    [ aere_error:nothing_to_remove()
      || State == State1], %% FIXME can be done smarter
    free_names(State, string:split(Inp, aere_parse:whitespaces()));
process_input(S, pwd, _) ->
    shell_default:pwd(),
    S;
process_input(S, cd, Inp) ->
    shell_default:cd(Inp),
    S;
process_input(S, ls, _) ->
    shell_default:ls(),
    S;
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
            {string:trim(M), NS};
        finito -> finito
    end;
process_input(_, _, _) ->
    aere_error:undefined_command().

register_letval(S0 = #repl_state{ letvals = Letvals
                                , tracked_contracts = Cons
                                , options = Opts
                                }, Pat, Expr0) ->
    Expr = unfold_aliases(S0, Expr0),
    {S1, PName} = make_provider_name(S0, Pat),
    Provider = aere_mock:letval_provider(S1, PName, Expr),
    TProvider = aere_sophia:typecheck(Provider),
    TProviderD = aere_sophia:typecheck(Provider, [dont_unfold]),
    {[], Type} = aere_sophia:type_of(TProviderD, ?LETVAL_GETTER(PName)),
    {{Ref, _}, S2} = build_deploy_contract("no_source", TProvider, {}, Opts, S1),
    {Cons1, Letvals1} = remove_references(aere_sophia:get_pat_ids(Pat), Cons, Letvals),
    S3 = S2#repl_state{ letvals = [{{PName, Ref}, {Pat, Type}}|Letvals1]
                      , tracked_contracts = Cons1
                      },
    assert_integrity(S3).

register_letfun(S, []) ->
    S; % efficency
register_letfun(S0 = #repl_state{ letfuns = Letfuns
                                , letvals = Letvals
                                , tracked_contracts = Cons
                                }, Funs0) ->
    Funs = unfold_aliases(S0, Funs0),
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
                        fun(NN) -> ?IF(NN == Name, NewName, NN) end,
                    {SS, [{FName,
                           {[case F of
                                 {fun_decl, A, {id, AName, Fn}, RT} ->
                                     {fun_decl, A, {id, AName, UpdateName(Fn)}, RT};
                                 {letfun, A, {id, AName, Fn}, Args, RT, B} ->
                                     {letfun, A, {id, AName, UpdateName(Fn)}, Args, RT,
                                      aere_sophia:replace(B, id, Name, NewName)}
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
    assert_integrity(S2).

make_provider_name(S0, Pat) ->
    Ids = aere_sophia:get_pat_ids(Pat),
    {S1, Sup} = next_sup(S0),
    {S1, string:join(Ids, "_") ++ io_lib:format("#~p", [Sup])}.

make_shadowed_fun_name(S0, Name) ->
    {S1, Sup} = next_sup(S0),
    {S1, Name ++ "#" ++ integer_to_list(Sup)}.

remove_references(Names, Cons, LetVals) ->
    LetVals1 =
        [ {{Provider, ProvRef}, {NewPat, Type}}  %% removing letvals shadowed by rec and args
          || {{Provider, ProvRef}, {Pat, Type}} <- LetVals,
             NewPat <- [lists:foldl(
                          fun(V, P) -> aere_sophia:replace(P, id, V, "_")
                          end, Pat, Names)],
             lists:any(fun({id, _, "_"}) -> false;  %% If everything is removed, why even consider it?
                          (_) -> true
                       end, aere_sophia:get_pat_ids(NewPat))
        ],
    Cons1 =
        [ ?IF(lists:member(CName, Names), {CName, {shadowed_contract, ConRef, I}}, C)
          || C = {CName, {_, ConRef, I}} <- Cons
        ],
    LetvalNames = [N || {_, {Pat, _}} <- LetVals, N <- aere_sophia:get_pat_ids(Pat)],
    Cons2 = lists:filter(  %% If shadowed by nothing then we remove it
              fun({CName, {shadowed_contract, _, _}}) ->
                      lists:member(CName, LetvalNames) or not lists:member(CName, Names);
                 (_) -> true
              end, Cons1),
    {Cons2, LetVals1}.

free_names(State, Names) ->
    free_names(State, Names, fun(X) -> X end).
free_names(State = #repl_state{ letfuns = Letfuns
                              , letvals = Letvals
                              , tracked_contracts = Cons
                              }, Names, Callback) ->
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
        [] -> Callback(State1);
        AdditionalNames ->
            #repl_question
                { text = [ aere_color:emph("This will require removing following entities: ")
                         , aere_color:yellow(string:join(AdditionalNames, ", "))
                         , aere_color:emph(". Proceed?")
                         ]
                , options = [{"y", ?LAZY(State1)}, {"n", ?LAZY(State)}]
                , default = "y"
                , callback = Callback
                }
    end.


name_status(#repl_state
            { letvals = LetDefs
            , letfuns = LocFuns
            , tracked_contracts = TCons
            }, Name) ->
    % Behold, the state-of-the-art solution is approaching!
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
register_includes(State, []) ->
    State; %% This case may be necessary to break check-loops
register_includes(State = #repl_state{ include_ast = Includes
                                     , include_hashes = Hashes
                                     , include_files = PrevFiles
                                     }
                 , Files) ->
    Trimmed = lists:map(fun string:trim/1, Files),
    NoDups = Trimmed -- PrevFiles,  % without duplicates
    State1 = case Trimmed -- NoDups of
                 [] -> State;
                 Duplicates ->
                     ColoredD = lists:flatten([" " ++ D || D <- Duplicates]),
                     warning(State, ["Following files are already included: ", ColoredD])
    end,
    IncludingContract = lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, NoDups),
    {Addition, NewHashes} = aere_sophia:parse_file(IncludingContract, Hashes, [keep_included]),
    NewIncludes = Includes ++ Addition,
    State2 = State1#repl_state{ include_ast    = NewIncludes
                              , include_hashes = NewHashes
                              , include_files  = NoDups ++ PrevFiles
                              },

    Colored = aere_color:yellow(lists:flatten([" " ++ F || F <- NoDups])),
    IncludeWord = case NoDups of
                      []  -> "nothing";
                      [_] -> "include";
                      _   -> "includes"
                  end,
    Msg = ["Registered ", IncludeWord, Colored],
    {Msg, assert_integrity(State2)}.

register_tracked_contract(State0 = #repl_state
                          { letvals = Letvals
                          , type_alias_map = TypeMap
                          , options = Opts
                          }, MaybeRefName, Src) ->
    % perform typecheck and prepare ACI
    Ast = aere_sophia:parse_file(binary_to_list(Src), []),
    TAstUnfolded = aere_sophia:typecheck(Ast, [dont_unfold]),
    TAst = aere_sophia:typecheck(Ast),
    BCode = aere_sophia:compile_contract(fate, binary_to_list(Src), TAst),
    Interface = {contract, _, {con, _, StrDeclName}, _}
        = aere_sophia:generate_interface_decl(TAstUnfolded),

    % Generate the reference name by the contract name if not provided
    RefName =
        case MaybeRefName of
            none ->
                [FstChar|OrigNameRest] = StrDeclName,
                NameWithIndex =
                    fun(X) -> string:lowercase([FstChar]) ++
                                  OrigNameRest ++
                                  case X of
                                      -1 -> "";
                                      _ -> integer_to_list(X)
                                  end
                    end,
                MakeIndex =
                    fun R(X) ->
                            case name_status(State0, NameWithIndex(X)) of
                                free -> X;
                                _    -> R(X + 1)
                            end
                    end,
                NameWithIndex(MakeIndex(-1));
            _ -> MaybeRefName
        end,

    % Generate the unique name for the contract declaration
    {State1, ActualName} =
        (fun Retry(State0_0) ->
                 {State0_1, Sup} = next_sup(State0_0),
                 TryName =
                     "REPL_" ++ integer_to_list(Sup) ++ "_" ++ StrDeclName,
                 case [bad || {_, {contract, NameConflict}} <- TypeMap,
                              NameConflict == TryName] of
                     [] -> {State0_1, TryName};
                     _ -> Retry(State0_1)
                 end
         end)(State0),

    Interface1 =
        begin
            {contract, IAnn, {con, INAnn, _}, IDecl} = Interface,
            {contract, IAnn, {con, INAnn, ActualName}, IDecl}
        end,


    {{ConAddr, DeployGas}, State2} = deploy_contract(BCode, {}, Opts, State1),
    DepGasStr = ?IF(Opts#options.display_deploy_gas,
                    [ aere_color:yellow("\ndeploy gas: "), io_lib:format("~p", [DeployGas])],
                    ""
                   ),
    {Cons1, Letvals1} = remove_references([RefName], State2#repl_state.tracked_contracts, Letvals),
    State3 = State2#repl_state
        { tracked_contracts =
              [{ RefName
               , {tracked_contract, ConAddr, Interface1}} | Cons1]
        , type_alias_map =
              [{StrDeclName, {contract, ActualName}}|proplists:delete(StrDeclName, TypeMap)]
        , letvals = Letvals1
        },
    {[ aere_color:green(RefName ++ " : " ++ StrDeclName)
     , " was successfully deployed", DepGasStr]
    , assert_integrity(State3)
    }.


register_typedef(State, {type_def, _, {id, NAnn, StrName}, Args, Def}) ->
    State0 = State#repl_state
        { type_alias_map = proplists:delete(StrName, State#repl_state.type_alias_map) },
    % search for free namespace
    {State1, NSName} =
        (fun Retry(State0_0) ->
                 {State0_1, Sup} = next_sup(State0_0),
                 TryName = "TYPEDEF_" ++ integer_to_list(Sup),
                 Mock = aere_mock:simple_query_contract(State0_1, [{id, aere_mock:ann(), "state"}]),
                 Namespaces = [N || {namespace, _, {con, _, N}, _} <- Mock],
                 Contracts = [N || {contract, _, {con, _, N}, _} <- Mock],
                 ?IF(lists:member(TryName, Namespaces ++ Contracts),
                     Retry(State0_1),
                     {State0_1, TryName}
                    )
         end)(State0),
    State2 = State1#repl_state
        { typedefs = [{ {qid, NAnn, [NSName, StrName]}
                      , {Args, unfold_aliases(State1, Def)}}|State1#repl_state.typedefs]
        , type_alias_map =
              [ {StrName, {typedef, Args, NSName, unfold_aliases(State1, Def)}}
                | State1#repl_state.type_alias_map
              ]
        },
    assert_integrity(State2).


unfold_aliases(#repl_state{type_alias_map = TypeMap}, Obj) ->
    Run = fun R([{Name, {typedef, _, Ns, Def}}|Rest], O) ->
                  O1 = aere_sophia:replace(
                         O, type, Name, {qid, aere_mock:ann(), [Ns, Name]}),
                  case Def of
                      {variant_t, Constrs} ->
                          R(lists:zip([Ns || _ <- Constrs], Constrs) ++ Rest, O1);
                      _ -> R(Rest, O1)
                  end;
              R([{Name, {contract, IName}}|Rest], O) ->
                  O1 = aere_sophia:replace(
                         O, type, Name, IName),
                  R(Rest, O1);
              R([{Ns, {constr_t, _, {con, _, Name}, _}}|Rest], O) ->
                  O1 = aere_sophia:replace(
                         O, con, Name, {qcon, aere_mock:ann(), [Ns, Name]}),
                  R(Rest, O1);
              R([], O) -> O
          end,
    Run(TypeMap, Obj).


warning(State = #repl_state{warnings = Ws}, W) ->
    State#repl_state{warnings = [W|Ws]}.


build_deploy_contract(Src, TypedAst, Args, Options, State) ->
    Code = aere_sophia:compile_contract(fate, Src, TypedAst),
    deploy_contract(Code, Args, Options, State).


deploy_contract(ByteCode, Args, Options, State = #repl_state{ chain_state = S0
                                                            , user_account = Owner
                                                            }) ->
    aere_chain:state(S0),
    Serialized = aect_sophia:serialize(ByteCode, aere_version:contract_version()),
    try aere_chain:create_contract(Owner, Serialized, Args, Options, S0) of
        {Result, S1} -> {Result, State#repl_state{chain_state = S1}}
    catch error:Reason ->
            ReasonS = if is_binary(Reason) -> binary_to_list(Reason);
                         is_list(Reason) -> Reason;
                         true -> io_lib:format("~p", [Reason])
                      end,
            aere_error:contract_creation_error(ReasonS)
    end.


eval_contract(Src, Ast, S1 = #repl_state{options = Options}) ->
    TypedAst = aere_sophia:typecheck(Ast),
    RetType = aere_response:convert_type( build_type_map(TypedAst)
                                        , element(2, aere_sophia:type_of(TypedAst, ?USER_INPUT))),
    Owner = S1#repl_state.user_account,
    {{Con, GasDeploy}, S2} = build_deploy_contract(Src, TypedAst, {}, Options, S1),
    CS1 = S2#repl_state.chain_state,
    {{Resp, GasCall}, CS2} =
        aere_chain:call_contract( Owner, Con, list_to_binary(?USER_INPUT)
                                  , RetType, {}, Options, CS1),
    S3 = S2#repl_state{chain_state = CS2},

    PPResp = prettypr:format(aere_response:pp_response(Resp)),
    DeployGasStr =
        ?IF(Options#options.display_deploy_gas,
            [ aere_color:yellow("\ndeploy gas: "), io_lib:format("~p", [GasDeploy])],
            ""),
    CallGasStr =
        ?IF(Options#options.display_call_gas,
            [ aere_color:yellow("\ncall gas: "), io_lib:format("~p", [GasCall])],
            ""),
    { S3#repl_state{ user_contracts = [Con|S3#repl_state.user_contracts] }
    , [PPResp, DeployGasStr, CallGasStr]}.


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

set_option(State = #repl_state{options = Opts}, Prop, Val) ->
    Parse =
        case Prop of
            "display-gas" -> ?ParseOptionBool(display_call_gas);
            "display-deploy-gas" -> ?ParseOptionBool(display_deploy_gas);
            "silent" -> ?ParseOptionBool(silent);
            "display-unit" -> ?ParseOptionBool(display_unit);
            "balance" ->
                try list_to_integer(Val) of
                    ValInt ->
                        CS0 = State#repl_state.chain_state,
                        PK = State#repl_state.user_account,
                        CS1 = aere_chain:update_balance(ValInt, PK, CS0),
                        { state
                        , State#repl_state{chain_state = CS1}
                        }
                catch error:badarg -> aere_error:bad_option(["integer"])
                end;
            "colors" ->
                case Val of
                    "none" -> {options, Opts#options{colors = none}};
                    "default" -> erase(wololo), {options, Opts#options{colors = default}};
                    "no-emph" -> erase(wololo), {options, Opts#options{colors = no_emph}};
                    _ -> aere_error:bad_option(["default", "none", "no-emph"])
                end;
            "call-gas" -> ?ParseOptionInt(gas);
            "call-value" -> ?ParseOptionInt(call_value);
            "state" ->
                Stmts = aere_sophia:parse_body(Val),
                Cont = fun(State1) ->
                               TExprAst = aere_sophia:typecheck(aere_mock:simple_query_contract(State1, Stmts)),
                               {_, Type} = aere_sophia:type_of(TExprAst, ?USER_INPUT),
                               Mock = aere_mock:chained_initial_contract(State1, Stmts, Type),
                               TMock = aere_sophia:typecheck(Mock),
                               {{Key, _}, State2} = build_deploy_contract("no_src", TMock, {}, Opts, State1),
                               State2#repl_state
                                   { user_contract_state_type = Type
                                   , user_contracts = [Key]
                                   }
                       end,
                %% Will work even if it's a question actually
                {state, free_names(State, ["state", "put"], Cont)};
            _ -> aere_error:unknown_option(Prop)
        end,
    case Parse of
        {options, NewOpts} ->
            State#repl_state{options = NewOpts};
        {state, NewState} ->
            NewState;
        {options, Msg, NewOpts} ->
            {Msg, State#repl_state{options = NewOpts}};
        {state, Msg, NewState} ->
            {Msg, NewState}
    end.

build_type_map(Ast) ->
    build_type_map([], Ast, #{}).
build_type_map(_Scope, [], Acc) ->
    Acc;
build_type_map(Scope, [{namespace, _, {con, _, Name}, Defs} | Rest], Acc) ->
    build_type_map(Scope, Rest, build_type_map(Scope ++ [Name], Defs, Acc));
build_type_map(Scope, [{contract, _, {con, _, Name}, Defs} | Rest], Acc) ->
    build_type_map(Scope, Rest, build_type_map(Scope ++ [Name], Defs, Acc));
build_type_map(Scope, [{type_def, _, {id, _, Name}, Args, {variant_t, Cons}} | Rest], Acc) ->
    build_type_map(Scope, Rest, Acc#{Scope ++ [Name] => {variant, Args, Cons}});
build_type_map(Scope, [{type_def, _, {id, _, Name}, Args, {record_t, Fields}} | Rest], Acc) ->
    build_type_map(Scope, Rest, Acc#{Scope ++ [Name] => {record, Args, Fields}});
build_type_map(Scope, [_|Rest], Acc) ->
    build_type_map(Scope, Rest, Acc).

next_sup(State = #repl_state{supply=S}) ->
    {State#repl_state{supply=S+1}, S}.

assert_integrity(S) ->
    TestMock = aere_mock:chained_query_contract(S, [{tuple, aere_mock:ann(), []}]),
    aere_sophia:typecheck(TestMock),
    S.
