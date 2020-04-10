%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ register_includes/2, init_state/0, process_string/2
        , remove_references/3, answer/2, question_to_response/1
        , print_msg/2, render_msg/2, banner/0, destroy_warnings/1
        ]).

-include("aere_repl.hrl").
-include("aere_macros.hrl").


-spec default_options() -> options().
default_options() ->
    #options{ display_call_gas = false
            , display_deploy_gas = false
            , gas = 1000000
            , height = 1
            , call_value = 0
            , colors = none
            , silent = false
            , display_unit = false
            }.


-spec init_state() -> repl_state().
init_state() ->
    {PK, ChainState} = aere_chain:new_account(100000000000000000000000000000, #{}),
    {ok, CWD} = file:get_cwd(),
    #repl_state{ include_ast = []
               , include_hashes = sets:new()
               , include_files = []
               , options = default_options()
               , blockchain_state = ChainState
               , user_contract_state_type = {tuple_t, [], []}
               , tracked_contracts = []
               , letvals = []
               , letfuns = []
               , typedefs = []
               , type_alias_map = []
               , user_account = PK
               , warnings = []
               , cwd = CWD
               , supply = 0
               }.


-spec banner() -> string().
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


%% Extract warnings from state and clear them
-spec destroy_warnings(repl_state()) -> {repl_state(), list(string())}.
destroy_warnings(State = #repl_state{warnings = Ws}) ->
    {State#repl_state{warnings = []}, Ws}.


%% Renders colored and untrimmed text into a string
-spec render_msg(repl_state(), colored()) -> string().
render_msg(#repl_state{options = Opts}, Msg) ->
    render_msg(Opts, Msg);
render_msg(O = #options{}, Msg) ->
    lists:flatten(aere_color:render_colored(O, Msg)).


%% Renders and prints message
-spec print_msg(repl_state(), colored()) -> ok.
print_msg(_, "") -> ok;
print_msg(_, "&#wololo#&") ->
    put(wololo, wololo), ok;
print_msg(S, M) ->
    Render = render_msg(S, M),
    io:format("~s\n", [string:trim(Render, both, aere_parse:whitespaces())]).


%% String describing possible answers accepted by the question
-spec valid_question_options_str(repl_question()) -> string().
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


%% Turn a question into a remote response.
%% Used to inform the client about the question and keeping the
%% callback on the server side
-spec question_to_response(repl_question()) -> repl_response().
question_to_response( Q = #repl_question
                      { text = Text
                      }) ->
    #repl_response
        { output = [Text, " ", valid_question_options_str(Q)]
        , warnings = []
        , status = ask
        }.

%% Answer the question. Returns proper response if succeeded or
%% another question if the answer is not satisfying
-spec answer(repl_question(), string()) -> repl_question() | repl_state().
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


%% state + input = response
-spec process_string(repl_state(), binary() | string())
                    -> repl_response() | repl_question() | finito.
process_string(State, String) when is_binary(String) ->
    process_string(State, binary_to_list(String));
process_string(State, String) ->
    Proc = aere_parse:dispatch(String),
    handle_dispatch(State, Proc).


%% Handle result of REPL command dispatcher
-spec handle_dispatch(repl_state(), skip | {ok, {command, string()}})
                     -> repl_response() | repl_question().
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
    #repl_response{output = "&#wololo#&", warnings = [], status = {success, State}};
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


%% Specific reactions to commands and inputs
-spec process_input(repl_state(), aere_parse:command(), string()) ->
          finito | {string(), repl_state()} | repl_state().
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
            eval_contract(I, Mock, State);
        [{include, _, {string, _, Inc}}] ->
            register_includes(State, [binary_to_list(Inc)]);
        [{letval, _, Pat, Expr}] -> register_letval(State, Pat, Expr);
        [FD = {fun_decl, _, _Name, _Type}] -> register_letfun(State, [FD]);
        [FD = {letfun, _, _Name, _Args, _RetType, _Body}] -> register_letfun(State, [FD]);
        [{block, _, Funs}] -> register_letfun(State, Funs);
        [TDf] when element(1, TDf) =:= type_def ->
            register_typedef(State, TDf);

        [TDc] when element(1, TDc) =:= type_decl ->
            throw(aere_error:unsupported_decl(type_decl));
        [Con] when element(1, Con) =:= contract ->
            throw(aere_error:unsupported_decl(contract));
        [Ns] when element(1, Ns) =:= namespace ->
            throw(aere_error:unsupported_decl(namespace));
        [_|_] ->
            throw(aere_error:unsupported_decl(multidecl))
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
            throw(aere_error:uninclude_error(Msg))
    end,
    {"Unregistered all includes", State1};
process_input(State, set, Inp) ->
    {Prop, Val0} = lists:splitwith(fun(X) -> X /= $  end, Inp),
    Val = string:trim(Val0),
    set_option(State, Prop, Val);
process_input(State = #repl_state {cwd = CWD}, deploy, Inp) ->
    {File, MaybeRefName} =
        case string:tokens(Inp, aere_parse:whitespaces()) of
            [] -> throw(aere_error:no_file_deploy());
            [F] -> {F, none};
            [F, "as", N] ->
                Valid = fun(C) -> ((C >= $a) and (C =< $z))
                                      or ((C >= $A) and (C =< $Z))
                                      or ((C >= $0) and (C =< $9))
                                      or (C == $_)
                        end,
                [throw(aere_error:bad_deploy_name())
                 || (lists:nth(1, N) < $a) or (lists:nth(1, N) > $z)],
                [throw(aere_error:bad_deploy_name())
                 || not lists:all(Valid, N)],
                {F, N};
            _ -> throw(aere_error:parse_deploy())
        end,
    Src = case file:read_file(filename:join(CWD, File)) of
              {ok, Src_} -> Src_;
              {error, Reason} -> throw(aere_error:file_error(File, Reason))
          end,
    register_tracked_contract(State, MaybeRefName, Src);
process_input(State, rm, Inp) ->
    State1 = free_names(State, string:split(Inp, aere_parse:whitespaces())),
    [ throw(aere_error:nothing_to_remove())
      || State#repl_state.letvals == State1#repl_state.letvals
         andalso State#repl_state.tracked_contracts == State1#repl_state.tracked_contracts
         andalso State#repl_state.letfuns == State1#repl_state.letfuns
    ],
    free_names(State, string:split(Inp, aere_parse:whitespaces()));
process_input(S = #repl_state{cwd = CWD}, pwd, _) ->
    {CWD, S};
process_input(S = #repl_state{cwd = CWD}, cd, Inp) ->
    Target = filename:join(CWD, Inp),
    [throw(aere_error:file_error(Target, enoent)) || not filelib:is_dir(Target)],
    S#repl_state{cwd = Target};
process_input(S = #repl_state{cwd = CWD}, ls, _) ->
    {ok, Files} = file:list_dir_all(CWD),
    { case Files of
          [] -> "";
          [F] -> F;
          [F|More] ->
              [F|[[", ", Fx] || Fx <- More]]
      end
    , S};
process_input(_, _, _) ->
    throw(aere_error:undefined_command()).

-spec register_letval(repl_state(), aeso_syntax:pat(), aeso_syntax:expr())
                     -> repl_state().
register_letval(S0 = #repl_state{ letvals = Letvals
                                , tracked_contracts = Cons
                                , options = Opts
                                }, Pat, Expr0) ->
    Expr = unfold_aliases(S0, Expr0),
    {S1, PName} = make_provider_name(S0, Pat),
    Provider = aere_mock:letval_provider(S1, PName, Expr),

    %% Typecheck for deploy
    TProvider = aere_sophia:typecheck(Provider),
    %% Typecheck for declaration of the provider
    TProviderD = aere_sophia:typecheck(Provider, [dont_unfold]),
    {[], Type} = aere_sophia:type_of(TProviderD, ?LETVAL_GETTER(PName)),

    {{Ref, _}, S2} = build_deploy_contract("no_source", TProvider, {}, Opts, S1),

    {Cons1, Letvals1} = remove_references(aere_sophia:get_pat_ids(Pat), Cons, Letvals),
    S3 = S2#repl_state{ letvals = [{{PName, Ref}, {Pat, Type}}|Letvals1]
                      , tracked_contracts = Cons1
                      },
    assert_integrity(S3).

-spec register_letfun(repl_state(), list(aeso_syntax:decl())) -> repl_state().
register_letfun(S, []) ->
    S; % I couldn't even extract the name
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
        "init" -> throw(aere_error:forbidden_id(Name));
        _ -> ok
    end,
    {Cons1, Letvals1} = remove_references([Name], Cons, Letvals),
    {S1, Shadowed} =
        case proplists:get_value(Name, Letfuns, none) of
            none -> {S0, Letfuns};
            Dupl ->
                begin %% Shadow duplicates
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

-spec make_provider_name(repl_state(), aeso_syntax:pat()) -> {repl_state(), string()}.
make_provider_name(S0, Pat) ->
    Ids = aere_sophia:get_pat_ids(Pat),
    {S1, Sup} = next_sup(S0),
    {S1, string:join(Ids, "_") ++ io_lib:format("#~p", [Sup])}.

-spec make_shadowed_fun_name(repl_state(), string()) -> {repl_state(), string()}.
make_shadowed_fun_name(S0, Name) ->
    {S1, Sup} = next_sup(S0),
    {S1, Name ++ "#" ++ integer_to_list(Sup)}.

%% Shadows out or removes defined tracked contracts or local variables
-spec remove_references(list(string()), list(tracked_contract()), list(letval()))
                       -> {list(tracked_contract()), list(letval())}.
remove_references(Names, Cons, LetVals) ->
    LetVals1 =
        [ {{Provider, ProvRef}, {NewPat, Type}}
          || {{Provider, ProvRef}, {Pat, Type}} <- LetVals,
             NewPat <- [lists:foldl(
                          fun(V, P) -> aere_sophia:replace(P, id, V, "_")
                          end, Pat, Names)],
             %% If no id is used then remove the reference
             lists:any(fun({id, _, "_"}) -> false;
                          (_) -> true
                       end, aere_sophia:get_pat_ids(NewPat))
        ],
    Cons1 =
        [ ?IF(lists:member(CName, Names), {CName, {shadowed_contract, ConRef, I}}, C)
          || C = {CName, {_, ConRef, I}} <- Cons
        ],
    %% Get letvals after first cleansing
    LetvalNames = [N || {_, {Pat, _}} <- LetVals, N <- aere_sophia:get_pat_ids(Pat)],
    Cons2 = lists:filter(  %% If shadowed by nothing then we remove it
              fun({CName, {shadowed_contract, _, _}}) ->
                      lists:member(CName, LetvalNames) or not lists:member(CName, Names);
                 (_) -> true
              end, Cons1),
    {Cons2, LetVals1}.

%% Removes variables along with everything that depends on them
-spec free_names(repl_state(), list(string())) -> repl_state() | repl_question().
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


-spec name_status(repl_state(), string()) -> free | taken.
name_status(#repl_state
            { letvals = LetDefs
            , letfuns = LocFuns
            , tracked_contracts = TCons
            }, Name) ->
    % The best 'if' you will ever see
    case { proplists:is_defined(Name, LetDefs)
         , proplists:is_defined(Name, LocFuns)
         , proplists:is_defined(Name, TCons)
         } of
        {false, false, false} -> free;
        _ -> taken
    end.


-spec register_includes(repl_state(), list(string())) -> repl_state().
register_includes(State, []) ->
    State; %% This is necessary to break check-loops
register_includes(State = #repl_state{ include_ast = Includes
                                     , include_hashes = Hashes
                                     , include_files = PrevFiles
                                     , cwd = CWD
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
    IncludingContract =
        lists:flatmap(fun(I) -> "include \"" ++ I ++ "\"\n" end, NoDups),
    {Addition, NewHashes} =
        aere_sophia:parse_file( IncludingContract
                              , Hashes
                              , aeso_compiler:add_include_path(
                                  %% this "mock" is because `add_include_path` removes head
                                  filename:join(CWD, "mock"), [keep_included])),
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

-spec register_tracked_contract(repl_state(), none | string(), binary())
                               -> {string(), repl_state()}.
register_tracked_contract(State = #repl_state
                          { letvals = Letvals
                          , options = Opts
                          , cwd = CWD
                          }, MaybeRefName, Src) ->

    % typecheck and prepare ACI
    Ast = aere_sophia:parse_file(binary_to_list(Src), aeso_compiler:add_include_path(CWD, [])),
    TAstUnfolded = aere_sophia:typecheck(Ast, [dont_unfold]),
    TAst = aere_sophia:typecheck(Ast),
    BCode = aere_sophia:compile_contract(fate, binary_to_list(Src), TAst),
    Interface = {contract, _, {con, _, StrDeclName}, _}
        = aere_sophia:generate_interface_decl(TAstUnfolded),
    State0 = State#repl_state
        {type_alias_map =
             proplists:delete(StrDeclName, State#repl_state.type_alias_map)},

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
                 case [bad || {_, {contract, NameConflict}} <-
                                  State0_1#repl_state.type_alias_map,
                              NameConflict == TryName] of
                     [] -> { State0_1#repl_state
                             {type_alias_map = [{StrDeclName, {contract, TryName}}
                                                | State0_1#repl_state.type_alias_map
                                               ]}
                           , TryName};
                     _ -> Retry(State0_1)
                 end
         end)(State0),

    Interface1 =
        begin
            {contract, IAnn, {con, INAnn, _}, IDecl} = unfold_aliases(State1, Interface),
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
        , letvals = Letvals1
        },
    {[ aere_color:green(RefName ++ " : " ++ StrDeclName)
     , " was successfully deployed", DepGasStr]
    , assert_integrity(State3)
    }.


-spec register_typedef(repl_state(), typedef()) -> repl_state().
register_typedef(State, {type_def, _, {id, NAnn, StrName}, Args, Def}) ->
    Constructors = case Def of
                       {variant_t, Constrs} ->
                           [C || {constr_t, _, {con, _, C}, _} <- Constrs];
                       _ -> []
                   end,
    FreeTypeMap =
        lists:foldl(
          fun(Con, Prev) ->
                  proplists:delete(Con, Prev)
          end,
          proplists:delete(StrName, State#repl_state.type_alias_map),
          Constructors
         ),
    State0 = State#repl_state
        { type_alias_map = FreeTypeMap },

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
    NewTypeMap =
        lists:foldr(
          fun(Constr, Prev) -> [{Constr, {constructor, NSName}}|Prev] end,
          [{StrName, {typedef, Args, NSName}} | FreeTypeMap],
          Constructors
         ),

    State2 = State1#repl_state
        { typedefs = [{ {qid, NAnn, [NSName, StrName]}
                      , {Args, unfold_aliases(State1, Def)}}|State1#repl_state.typedefs]
        , type_alias_map = NewTypeMap
        },
    assert_integrity(State2).


-spec unfold_aliases(repl_state(), any()) -> any().
unfold_aliases(#repl_state{type_alias_map = TypeMap}, Obj) ->
    Run = fun R([{Name, {typedef, _, Ns}}|Rest], O) ->
                  O1 = aere_sophia:replace(
                         O, type, Name, {qid, aere_mock:ann(), [Ns, Name]}),
                  R(Rest, O1);
              R([{Name, {contract, IName}}|Rest], O) ->
                  O1 = aere_sophia:replace(O, type, Name, IName),
                  R(Rest, O1);
              R([{Name, {constructor, Ns}}|Rest], O) ->
                  O1 = aere_sophia:replace(
                         O, con, Name, {qcon, aere_mock:ann(), [Ns, Name]}),
                  R(Rest, O1);
              R([], O) -> O
          end,
    Run(TypeMap, Obj).


-spec warning(repl_state(), string()) -> repl_state().
warning(State = #repl_state{warnings = Ws}, W) ->
    State#repl_state{warnings = [W|Ws]}.


-spec build_deploy_contract(string(), aeso_syntax:ast(), [any()], options(), repl_state())
                           -> {any(), repl_state()}.
build_deploy_contract(Src, TypedAst, Args, Options, State) ->
    Code = aere_sophia:compile_contract(fate, Src, TypedAst),
    deploy_contract(Code, Args, Options, State).


deploy_contract( ByteCode, Args, Options
               , State = #repl_state{ blockchain_state = S0
                                    , user_account = Owner
                                    }) ->
    aere_chain:state(S0),
    Serialized = aect_sophia:serialize(ByteCode, aere_version:contract_version()),
    try aere_chain:create_contract(Owner, Serialized, Args, Options, S0) of
        {Result, S1} -> {Result, State#repl_state{blockchain_state = S1}}
    catch error:Reason ->
            ReasonS = if is_binary(Reason) -> binary_to_list(Reason);
                         is_list(Reason) -> Reason;
                         true -> io_lib:format("~p", [Reason])
                      end,
            throw(aere_error:contract_creation_error(ReasonS))
    end.


eval_contract(Src, Ast, S1 = #repl_state{options = Options}) ->
    TypedAst = aere_sophia:typecheck(Ast),
    RetType = aere_response:convert_type( build_type_map(TypedAst)
                                        , element(2, aere_sophia:type_of(TypedAst, ?USER_INPUT))),
    Owner = S1#repl_state.user_account,
    {{Con, GasDeploy}, S2} = build_deploy_contract(Src, TypedAst, {}, Options, S1),
    CS1 = S2#repl_state.blockchain_state,
    {{Resp, GasCall}, CS2} =
        aere_chain:call_contract( Owner, Con, list_to_binary(?USER_INPUT)
                                  , RetType, {}, Options, CS1),
    S3 = S2#repl_state{blockchain_state = CS2},

    PPResp = prettypr:format(aere_response:pp_response(Resp)),
    DeployGasStr =
        ?IF(Options#options.display_deploy_gas,
            [ aere_color:yellow("\ndeploy gas: "), io_lib:format("~p", [GasDeploy])],
            ""),
    CallGasStr =
        ?IF(Options#options.display_call_gas,
            [ aere_color:yellow("\ncall gas: "), io_lib:format("~p", [GasCall])],
            ""),
    Out = [PPResp, DeployGasStr, CallGasStr],
    S4 = S3#repl_state{last_state_provider = Con},
    case {RetType, DeployGasStr, CallGasStr} of
        {{tuple, []}, "", ""} ->
            ?IF(S4#repl_state.options#options.display_unit, {Out, S4}, S4);
        _ -> {Out, S4}
    end.


-define(ParseOptionBool(Field),
        if
            Val =:= "true" orelse Val =:= 1 ->
                {options, Opts#options{Field = true}};
            Val =:= "false" orelse Val =:= 0 ->
                {options, Opts#options{Field = false}};
            true ->
                throw(aere_error:bad_option(["true", "false"]))
        end).
-define(ParseOptionInt(Field),
        try {options, Opts#options{Field = list_to_integer(Val)}}
        catch error:badarg -> throw(aere_error:bad_option(["integer"]))
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
                        CS0 = State#repl_state.blockchain_state,
                        PK = State#repl_state.user_account,
                        CS1 = aere_chain:update_balance(ValInt, PK, CS0),
                        { state
                        , State#repl_state{blockchain_state = CS1}
                        }
                catch error:badarg -> throw(aere_error:bad_option(["integer"]))
                end;
            "colors" ->
                case Val of
                    "none" -> {options, Opts#options{colors = none}};
                    "emph" -> erase(wololo), {options, Opts#options{colors = emph}};
                    "no-emph" -> erase(wololo), {options, Opts#options{colors = no_emph}};
                    _ -> throw(aere_error:bad_option(["emph", "none", "no-emph"]))
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
                                   , last_state_provider = Key
                                   }
                       end,
                {state, free_names(State, ["state", "put"], Cont)};
            _ -> throw(aere_error:unknown_option(Prop))
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

-spec build_type_map(aeso_syntax:ast()) -> #{list(string()) => aeso_syntax:type_def()}.
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

-spec assert_integrity(repl_state()) -> repl_state().
assert_integrity(S) ->
    TestMock = aere_mock:chained_query_contract(S, [{tuple, aere_mock:ann(), []}]),
    aere_sophia:typecheck(TestMock),
    S.
