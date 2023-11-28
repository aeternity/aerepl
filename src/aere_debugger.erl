-module(aere_debugger).

-export([ add_breakpoint/3
        , delete_breakpoint/2
        , delete_breakpoint/3
        , resume_eval/2
        , lookup_variable/2
        , dump_variables/1
        , source_location/1
        , stacktrace/1
        ]).


-spec add_breakpoint(ReplState, FileName, Line) -> ReplState
    when ReplState :: aere_repl_state:state(),
         FileName  :: string(),
         Line      :: integer().

add_breakpoint(State, FileName, Line) ->
    LoadedFiles = maps:keys(aere_repl_state:loaded_files(State)),
    [ throw({repl_error, aere_msg:breakpoint_file_not_loaded(FileName)})
        || not lists:member(FileName, LoadedFiles) ],
    OldBPs = aere_repl_state:breakpoints(State),
    BP     = {FileName, Line},
    NewBPs = OldBPs ++ [BP],
    aere_repl_state:set_breakpoints(NewBPs, State).


-spec delete_breakpoint(ReplState, Index) -> ReplState | no_return()
    when ReplState :: aere_repl_state:state(),
         Index     :: integer().

delete_breakpoint(State, Index) ->
    BPs = aere_repl_state:breakpoints(State),
    [ throw({repl_error, aere_msg:breakpoint_out_of_range(Index)})
        || Index < 1 orelse Index > length(BPs) ],
    {Left, [_ | Right]} = lists:split(Index - 1, BPs),
    aere_repl_state:set_breakpoints(Left ++ Right, State).


-spec delete_breakpoint(ReplState, File, Line) -> ReplState | no_return()
    when ReplState :: aere_repl_state:state(),
         File      :: string(),
         Line      :: pos_integer().

delete_breakpoint(State, File, Line) ->
    BPs = aere_repl_state:breakpoints(State),
    NewBPs = [B || B = {F, L} <- BPs, F =/= File orelse L =/= Line],
    [ throw({repl_error, aere_msg:breakpoint_wrong_location(File, Line)})
        || length(BPs) == length(NewBPs) ],
    aere_repl_state:set_breakpoints(NewBPs, State).


-spec resume_eval(ReplState, ResumeKind) -> EngineState | no_return()
    when ReplState   :: aere_repl_state:state(),
         EngineState :: aefa_engine_state:state(),
         ResumeKind  :: continue | stepin | stepout | stepover.

resume_eval(RS, Kind) ->
    case aere_repl_state:blockchain_state(RS) of
        {abort, _} ->
            Chain = aere_repl_state:chain_api(RS),
            NewRS = aere_repl_state:set_blockchain_state({ready, Chain}, RS),
            {aere_msg:contract_exec_ended(), NewRS};
        _ ->
            ES0 = breakpoint_engine_state(RS),
            ES1 = resume(ES0, Kind),
            aere_repl:eval_handler(RS, aere_fate:resume_contract_debug(ES1, RS))
    end.


-spec resume(EngineState, ResumeKind) -> EngineState
    when EngineState :: aefa_engine_state:state(),
         ResumeKind  :: continue | stepin | stepout | stepover.

resume(ES, Kind) ->
    CallStack = aefa_engine_state:call_stack(ES),
    Status =
        case Kind of
            K when K == stepover ; K == stepout -> {K, CallStack};
            _                                   -> Kind
        end,
    Info = aefa_debug:set_debugger_status(Status, aefa_engine_state:debug_info(ES)),
    aefa_engine_state:set_debug_info(Info, ES).


-spec lookup_variable(ReplState, VariableName) -> Renderable | no_return()
    when ReplState    :: aere_repl_state:state(),
         VariableName :: aere_theme:renderable(),
         Renderable   :: aere_theme:renderable().

lookup_variable(RS, VarName) ->
    aere_msg:output(lookup_variable_unthemed(RS, VarName)).


-spec lookup_variable_unthemed(ReplState, VariableName) -> string() | no_return()
    when ReplState    :: aere_repl_state:state(),
         VariableName :: aere_theme:renderable().

lookup_variable_unthemed(RS, VarName) ->
    ES = breakpoint_engine_state(RS),
    case aefa_debug:get_variable_register(VarName, aefa_engine_state:debug_info(ES)) of
        undefined ->
            throw({repl_error, aere_msg:undefined_variable(VarName)});
        Reg ->
            {Val, _} = aefa_fate:lookup_var(Reg, ES),
            io_lib:format("~p", [Val])
    end.


- spec dump_variables(ReplState) -> Renderable
    when ReplState  :: aere_repl_state:state(),
         Renderable :: aere_theme:renderable().

dump_variables(RS) ->
    ES = breakpoint_engine_state(RS),
    AllVars = aefa_debug:vars_registers(aefa_engine_state:debug_info(ES)),

    %% Filter out the variables with no mapped registers (variables defined in
    %% the repl instead of the called debugged code)
    VarsRegisters = maps:filter(fun(_, []) -> false; (_, _) -> true end, AllVars),

    Lookup = fun(K, _) ->
        io_lib:format("~s: ~s", [K, lookup_variable_unthemed(RS, K)]) end,
    Dump = maps:map(Lookup, VarsRegisters),

    aere_msg:output(lists:join("\n", maps:values(Dump))).


-spec source_location(ReplState) -> Source | no_return()
    when ReplState :: aere_repl_state:state(),
         Source    :: aere_theme:renderable().

source_location(RS) ->
    ES = breakpoint_engine_state(RS),

    {FileName, CurrentLine} = aefa_debug:debugger_location(aefa_engine_state:debug_info(ES)),

    File     = aere_files:read_file(FileName, RS),
    Lines    = string:split(File, "\n", all),
    LineSign =
        fun(Id) when Id == CurrentLine -> ">";
           (_)                         -> "|"
        end,
    MaxDigits     = length(integer_to_list(length(Lines))),
    FormatLineNum = fun(Num) -> string:right(integer_to_list(Num), MaxDigits) end,
    FormatLine    = fun(N, Ln) -> [LineSign(N), " ", FormatLineNum(N), " ", Ln] end,
    Enumerate     = fun(List) -> lists:zip(lists:seq(1, length(List)), List) end,
    NewLines      = [ FormatLine(Idx, Line)
                        || {Idx, Line} <- Enumerate(Lines)
                         , Idx < CurrentLine + 5, Idx > CurrentLine - 5 ],
    aere_msg:output(lists:join("\n", NewLines)).


-spec stacktrace(ReplState) -> Message | no_return()
    when ReplState :: aere_repl_state:state(),
         Message   :: aere_theme:renderable().

stacktrace(RS) ->
    ES = breakpoint_engine_state(RS),
    aere_msg:stacktrace(aere_fate:get_stack_trace(RS, ES)).


-spec breakpoint_engine_state(ReplState) -> EngineState | no_return()
    when ReplState   :: aere_repl_state:state(),
         EngineState :: aefa_engine_state:state().

breakpoint_engine_state(RS) ->
    case aere_repl_state:blockchain_state(RS) of
        {breakpoint, ES} -> ES;
        {abort, ES}      -> ES;
        _                -> throw({repl_error, aere_msg:not_at_breakpoint()})
    end.
