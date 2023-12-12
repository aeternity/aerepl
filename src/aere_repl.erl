%%%-------------------------------------------------------------------
%% @doc REPL for Sophia
%% @end
%%%-------------------------------------------------------------------

-module(aere_repl).

-export([ register_modules/2
        , infer_type/2
        , set_state/2
        , set_option/3
        , eval_code/2
        , update_filesystem_cache/2
        , load_modules/2
        , reload_modules/1
        , get_state/1
        , lookup_state/2
        , disassemble/2
        ]).

-include("aere_macros.hrl").

-type repl_state() :: aere_repl_state:state().

-type fun_ref() :: {definition, aeso_syntax:qid()}
                 | {local, aeso_syntax:name()}
                 | {deployed, aeso_syntax:expr(), aeso_syntax:name()}.

-type command_res() :: finish | repl_state() | no_return().
-type command_res(T) :: {T, repl_state()} | command_res().


infer_type(Expr, RS) ->
    Stmts = aere_sophia:parse_body(Expr),
    Contract = aere_mock:eval_contract(Stmts, RS),
    {TEnv, _} = aere_sophia:typecheck(Contract, [dont_unfold, allow_higher_order_entrypoints]),
    Type = aere_sophia:type_of_user_input(TEnv),
    Type.

eval_code(Expr, RS) ->
    Parse = aere_sophia:parse_top(Expr),
    case Parse of
        {body, Body} ->
            eval_expr(Body, RS);
        [{include, _, {string, _, Inc}}] ->
            {ok, register_include(Inc, RS)};
        [{letval, _, Pat, Body}] ->
            {ok, register_letval(Pat, Body, RS)};
        %% [{letfun, _, FName, Args, _, Body}] ->
        %%     {ok, register_letfun(FName, Args, Body, RS)};
        [{type_def, _, Name, Args, Body}] ->
            {ok, register_typedef(Name, Args, Body, RS)};
        _ -> error(too_much_stuff) %% FIXME
    end.

-spec set_state([aeso_syntax:stmt()], repl_state())
               -> command_res().
set_state(BodyStr, RS) ->
    Body         = aere_sophia:parse_body(BodyStr),
    Contract     = aere_mock:eval_contract(Body, RS),
    {TEnv, TAst} = aere_sophia:typecheck(Contract),
    Type         = aere_sophia:type_of_user_input(TEnv),
    ByteCode     = aere_sophia:compile_contract(TAst),
    RS1          = aere_fate:add_fun_symbols_from_code(RS, ByteCode),

    {#{value := StateVal}, RS2} = aere_fate:run_contract(ByteCode, RS1),

    RS3 = aere_repl_state:set_contract_state({Type, StateVal}, RS2),
    RS4 = aere_repl_state:set_vars([], RS3),
    RS5 = aere_repl_state:set_funs(#{}, RS4),
    RS5.

%%%------------------

-spec eval_expr([aeso_syntax:stmt()], repl_state())
               -> command_res(Result)
              when Result :: aere_fate:eval_debug_result().
eval_expr(Body, RS) ->
    Ast           = aere_mock:eval_contract(Body, RS),
    {TEnv, TAst}  = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode      = aere_sophia:compile_contract(TAst),
    RS1           = aere_fate:add_fun_symbols_from_code(RS, ByteCode),
    RS2           = aere_repl_state:set_type_env(TEnv, RS1),
    {Result, RS3} = aere_fate:run_contract_debug(ByteCode, RS2),
    {Result, RS3}.

%%%------------------

update_filesystem_cache(Fs, S0) when is_list(Fs) ->
    update_filesystem_cache(maps:from_list(Fs), S0);
update_filesystem_cache(Fs, S0) when is_map(Fs) ->
    case aere_repl_state:update_cached_fs(Fs, S0) of
        {ok, S1} ->
            S1;
        error ->
            throw({repl_error, aere_msg:filesystem_not_cached()})
    end.

load_modules([], S0) ->
    S0;
load_modules(Filenames, S0) ->
    Modules = aere_files:read_files(Filenames, S0),
    S1 = register_modules(Modules, S0),
    S2 = register_include(element(1, lists:last(Modules)), S1),
    S2.

reload_modules(RS) ->
    LdFiles  = aere_repl_state:loaded_files(RS),
    Modules = aere_files:read_files(maps:keys(LdFiles), RS),
    IncFiles = aere_repl_state:included_files(RS),
    RS1 = register_modules(Modules, RS),
    RS2 = lists:foldl(fun register_include/2, RS1, IncFiles),
    RS2.

-spec register_modules([{string(), binary()}], repl_state())
                      -> command_res().
register_modules(Modules, S0) ->
    FileMap = maps:from_list(Modules),
    [ begin
          Ast0 = aere_sophia:parse_file(File, []),
          aere_sophia:typecheck(aere_mock:ast_fillup_contract(Ast0)),
          File
      end
      || {_, File} <- Modules
    ],

    S1 = clear_context(S0),
    S2 = aere_repl_state:set_loaded_files(FileMap, S1),
    S2.

%% Removes all variables, functions, types and contracts
clear_context(S0) ->
    Contracts = [PK || {_Name, _Type, {contract, PK}} <- aere_repl_state:vars(S0)],
    Chain0 = get_ready_chain(S0),
    Chain1 = aere_fate:remove_contracts_from_chain(Chain0, Contracts),
    put(contract_code_cache, undefined),
    S1 = aere_repl_state:set_vars([], S0),
    S2 = aere_repl_state:set_funs(#{}, S1),
    S3 = aere_repl_state:set_typedefs([], S2),
    S4 = aere_repl_state:set_type_scope([], S3),
    S5 = aere_repl_state:set_included_files([], S4),
    S6 = aere_repl_state:set_included_code([], S5),
    S7 = aere_repl_state:set_blockchain_state({ready, Chain1}, S6),
    S7.

register_include(Include, S0) when is_binary(Include) ->
    register_include(binary:bin_to_list(Include), S0);
register_include(Include, S0) ->
    LdFiles  = aere_repl_state:loaded_files(S0),
    IncCode  = aere_repl_state:included_code(S0),
    IncFiles = aere_repl_state:included_files(S0),
    case maps:get(Include, maps:merge(default_loaded_files(S0), LdFiles), not_loaded) of
        not_loaded ->
            throw({repl_error, aere_msg:file_not_loaded(Include)});
        File ->
            S1 = case lists:member(Include, IncFiles) of
                     true -> S0;
                     false ->
                         IncludeSet = sets:from_list(
                                        [ {FName, Code} ||
                                            {FName, Code} <- maps:to_list(LdFiles),
                                            lists:member(FName, IncFiles)
                                        ]),
                         {Ast0, _IncludeSet1} = aere_sophia:parse_file(File, IncludeSet, [keep_included, {src_file, Include}]),
                         SX = aere_repl_state:set_included_files([Include|IncFiles], S0),
                         SY = aere_repl_state:set_included_code(IncCode ++ Ast0, SX),
                         SY
                 end,
            Ast = aere_mock:eval_contract([{tuple, aere_mock:ann(), []}], S1),
            aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
            S1
    end.

%%%------------------

-spec register_letval(aeso_syntax:pat(), aeso_syntax:expr(), repl_state())
                     -> command_res().
register_letval(Pat, Expr, S0) ->
    NewVars = lists:filter(
                fun(Var) -> Var /= "_" end,
                aeso_syntax_utils:used_ids([aere_mock:pat_as_decl(Pat)])),
    Ast = aere_mock:letval_contract(Pat, NewVars, Expr, S0),
    {TEnv, _} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints, dont_unfold]),
    %% TODO: Try to get TypedAstUnfolded without runnning the typechecker a second time
    {_, TypedAstUnfolded} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
    ByteCode = aere_sophia:compile_contract(TypedAstUnfolded),

    S00 = aere_repl_state:set_type_env(TEnv, S0),
    {#{value := Res}, S} = aere_fate:run_contract(ByteCode, S00),

    {Vals, Types, S1} =
        case NewVars of
            [_] ->
                T = aere_sophia:type_of_user_input(TEnv),
                {[Res], [T], S};
            _ ->
                {tuple_t, _, Ts} = aere_sophia:type_of_user_input(TEnv),
                {tuple, Vs} = Res,
                {tuple_to_list(Vs), Ts, S}
        end,
    NameMap = build_fresh_name_map(ByteCode),

    Vals1 = replace_function_name(Vals, NameMap),
    Vars1 = lists:zip3(NewVars, Types, Vals1),

    OldFuns = aere_repl_state:funs(S),
    NewFuns = generated_functions(ByteCode, NameMap),

    S2 = register_vars(Vars1, S1),
    S3 = aere_repl_state:set_funs(maps:merge(OldFuns, NewFuns), S2),

    S3.

%%%------------------

%% -spec register_letfun(aeso_syntax:id(), [aeso_syntax:pat()], [aeso_syntax:guarded_expr()], repl_state())
%%                      -> command_res().
%% register_letfun(Id = {id, _, Name}, Args, Body, S0) ->
%%     Ast = aere_mock:letfun_contract(Id, Args, Body, S0),
%%     {TEnv, TypedAst} = aere_sophia:typecheck(Ast, [allow_higher_order_entrypoints]),
%%     ByteCode = aere_sophia:compile_contract(TypedAst),

%%     Type = aere_sophia:type_of_user_input(TEnv),

%%     NameMap = build_fresh_name_map(ByteCode),

%%     Funs1 = generated_functions(ByteCode, NameMap),

%%     FunNewName = maps:get(aeb_fate_code:symbol_identifier(binary:list_to_bin(Name)), NameMap),
%%     FunVal = {tuple, {FunNewName, {tuple, {}}}}, %% TODO this is broken

%%     S1 = register_vars([{Name, Type, FunVal}], S0),
%%     S2 = aere_repl_state:set_funs(maps:merge(aere_repl_state:funs(S1), Funs1), S1),

%%     {aere_theme:error("Warning: defining functions in REPL is WIP and may be unstable."), S2}.

register_vars(NewVars, S) ->
    OldVars = aere_repl_state:vars(S),
    Filtered = [V || V = {Id, _, _} <- OldVars, [] =:= proplists:lookup_all(Id, NewVars)],
    aere_repl_state:set_vars(NewVars ++ Filtered, S).

build_fresh_name_map(ByteCode) ->
    Symbols = aeb_fate_code:symbols(ByteCode),
    AddedFuns = [FSym || {FSym, FName} <- maps:to_list(Symbols), not ?IS_REPL_ENTRYPOINT(FName)],
    NewAddedFuns = [binary:list_to_bin(erlang:ref_to_list(make_ref()))|| _ <- AddedFuns],
    maps:from_list(lists:zip(AddedFuns, NewAddedFuns)).

generated_functions(ByteCode, NameMap) ->
    Funs = maps:to_list(aeb_fate_code:functions(ByteCode)),
    Filter = fun({FName, Def}) ->
                     not ?IS_REPL_ENTRYPOINT(FName) andalso
                         {true, {maps:get(FName, NameMap, FName), replace_function_name(Def, NameMap)}}
             end,
    maps:from_list(lists:filtermap(Filter, Funs)).

replace_function_name({tuple, {FName, Closure}}, NameMap) when is_binary(FName) ->
    {tuple, {maps:get(FName, NameMap, FName), Closure}};
replace_function_name(E = {I, {immediate, FName}}, NameMap) when is_atom(I) andalso is_binary(FName) ->
    case atom_to_list(I) of
        "CALL" ++ _ -> {I, {immediate, maps:get(FName, NameMap, FName)}};
        _ -> E
    end;
replace_function_name(T, NameMap) when is_tuple(T) ->
    list_to_tuple(replace_function_name(tuple_to_list(T), NameMap));
replace_function_name([H|T], NameMap) ->
    [replace_function_name(H, NameMap)|replace_function_name(T, NameMap)];
replace_function_name(M, NameMap) when is_map(M) ->
    maps:from_list(replace_function_name(maps:to_list(M), NameMap));
replace_function_name(E, _) ->
    E.

-spec register_typedef(aeso_syntax:id(), [aeso_syntax:tvar()], aeso_syntax:typedef(), repl_state())
                      -> command_res().
register_typedef({id, _, Name}, Args, Def, S0) ->
    case Name of
        "state" -> throw({repl_error, aere_msg:state_typedef()});
        "event" -> throw({repl_error, aere_msg:event_typedef()});
        _       -> ok
    end,

    NamespaceName = ?TYPE_CONTAINER(aere_repl_state:query_nonce(S0)),

    TypeScope = aere_repl_state:type_scope(S0),
    TypeScope1 = proplists:delete(Name, TypeScope),
    S1 = aere_repl_state:set_type_scope(TypeScope1, S0),

    Def1 = unfold_types_in_type(Def, S1),

    % Check if definition is valid
    Ast = aere_mock:typedef_contract(Name, Args, Def1, S1),
    aere_sophia:typecheck(Ast),

    TypeDefEntry = {NamespaceName, Name, Args, Def1},
    TypeScope2 = [{Name, {NamespaceName, length(Args)}}|TypeScope1],

    TypeDefs  = aere_repl_state:typedefs(S0),

    S2 = aere_repl_state:set_typedefs([TypeDefEntry|TypeDefs], S1),
    S3 = aere_repl_state:set_type_scope(TypeScope2, S2),

    S3.

unfold_types_in_type(T, S0) ->
    Ast = aere_mock:type_unfold_contract(S0),
    {TEnv, _} = aere_sophia:typecheck(Ast, [dont_unfold]),
    TEnv1 = aeso_ast_infer_types:switch_scope([?MOCK_CONTRACT], TEnv),
    T1 = aeso_ast_infer_types:unfold_types_in_type(TEnv1, T),
    T1.

-spec disassemble(string(), repl_state()) -> term() | no_return(). %% -> bytecode
disassemble(What, S0) ->
    Chain0 = aere_repl_state:chain_api(S0),
    S1 = aere_repl_state:set_blockchain_state({ready, Chain0}, S0),
    case parse_fun_ref(What) of
        {deployed, Expr, Name} ->
            Contract = aere_mock:eval_contract(Expr, S1),
            {TEnv, TAst} = aere_sophia:typecheck(Contract),
            S1_0 = aere_repl_state:set_type_env(TEnv, S1),
            MockByteCode = aere_sophia:compile_contract(TAst),
            {#{value := {contract, Pubkey}}, S2} =
                aere_fate:run_contract(MockByteCode, S1_0),
            Chain = aere_repl_state:chain_api(S2),
            aere_fate:extract_fun_from_contract(Pubkey, Chain, Name);
        {definition, Id} ->
            Contract = aere_mock:eval_contract(Id, S1),
            {TEnv, TAst} = aere_sophia:typecheck(Contract, [allow_higher_order_entrypoints]),
            S1_0 = aere_repl_state:set_type_env(TEnv, S1),
            MockByteCode = aere_sophia:compile_contract(TAst),
            {#{value := {tuple, {FName, _}}}, _} = aere_fate:run_contract(MockByteCode, S1_0),
            aere_fate:extract_fun_from_bytecode(MockByteCode, FName);
        {local, _Name} ->
            error(not_supported);
        error ->
            throw({repl_error, aere_msg:bad_fun_ref()})
    end.

-spec parse_fun_ref(string()) -> fun_ref() | error.
parse_fun_ref(What) ->
    case aere_sophia:parse_body(What) of
        [{qid, _, _} = Qid]               -> {definition, Qid};
        [{id, _, Name}]                   -> {local, Name};
        [{proj, _, Contr, {id, _, Name}}] -> {deployed, Contr, Name};
        _                                 -> error
    end.

-spec default_loaded_files(repl_state()) -> #{string() => binary()} | no_return().
default_loaded_files(S) ->
    case get(aere_default_loaded_files) of
        undefined ->
            StdlibDir = aeso_stdlib:stdlib_include_path(),
            Files =
                [ File ||
                    File <- element(2, file:list_dir(StdlibDir)),
                    filename:extension(File) =:= ".aes"
                ],
            FileMap = maps:from_list(aere_files:read_files(Files, S)),
            put(aere_default_loaded_files, FileMap),
            FileMap;
        Files ->
            Files
    end.

get_ready_chain(RS) ->
    case aere_repl_state:blockchain_state(RS) of
        {ready, Chain} ->
            Chain;
        _ ->
            throw({repl_error, aere_msg:chain_not_ready()})
    end.

set_option(Option, Args, RS) ->
    Opts = aere_repl_state:options(RS),
    Locked = maps:get(locked_opts, Opts, []),
    LockedOption = lists:member(Option, [locked_opts|Locked]),
    LockedOption andalso throw({repl_error, aere_msg:locked_option()}),
    case aere_options:parse_option(Option, Args) of
        error ->
            throw({repl_error, aere_msg:option_usage(Option)});
        Val ->
            NewOpts = Opts#{Option => Val},
            aere_repl_state:set_options(NewOpts, RS)
    end.

get_state(RS) ->
    Vars  = aere_repl_state:vars(RS),
    Types = aere_repl_state:typedefs(RS),
    Opts  = aere_repl_state:options(RS),
    Files = aere_repl_state:loaded_files(RS),
    Incs  = aere_repl_state:included_files(RS),
    BPs   = aere_repl_state:breakpoints(RS),
    #{ "vars"        => Vars,
       %% "funs"        => /1, Funs},
       "types"       => Types,
       "options"     => Opts,
       "files"       => Files,
       "includes"    => Incs,
       "breakpoints" => BPs
     }.

lookup_state(RS, What) ->
    DataPack = get_state(RS),
    case maps:get(What, DataPack, unknown) of
        unknown ->
            throw({repl_error, aere_msg:list_unknown(maps:keys(DataPack))});
        Data ->
             Data
    end.
