-module(aere_debugger).

-export([ add_breakpoint/3
        , delete_breakpoint/2
        , resume_eval/2
        , lookup_variable/2
        , source_location/1
        , stacktrace/1
        ]).


-spec add_breakpoint(ReplState, FileName, Line) -> ReplState
    when ReplState :: aere_repl_state:state(),
         FileName  :: string(),
         Line      :: integer().

add_breakpoint(State, FileName, Line) ->
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


-spec resume_eval(ReplState, ResumeKind) -> EngineState | no_return()
    when ReplState   :: aere_repl_state:state(),
         EngineState :: aefa_engine_state:state(),
         ResumeKind  :: step | next | continue | finish.

resume_eval(RS, Kind) ->
    ES0 = breakpoint_engine_state(RS),
    ES1 = resume(ES0, Kind),
    aere_repl:eval_handler(RS, aere_fate:resume_contract_debug(ES1, RS)).


-spec resume(EngineState, ResumeKind) -> EngineState
    when EngineState :: aefa_engine_state:state(),
         ResumeKind  :: step | next | continue | finish.

resume(ES, Kind) ->
    CallStack = aefa_engine_state:call_stack(ES),
    Status =
        case Kind of
            K when K == next; K == finish -> {K, CallStack};
            _                             -> Kind
        end,
    aefa_engine_state:set_debugger_status(Status, ES).


-spec lookup_variable(ReplState, VariableName) -> string() | no_return()
    when ReplState    :: aere_repl_state:state(),
         VariableName :: aere_theme:renderable().

lookup_variable(RS, VarName) ->
    ES = breakpoint_engine_state(RS),
    case aefa_engine_state:get_variable_register(VarName, ES) of
        undefined ->
            throw({repl_error, aere_msg:undefined_variable(VarName)});
        Reg ->
            {Val, _} = aefa_fate:lookup_var(Reg, ES),
            aere_msg:output(io_lib:format("~p", [Val]))
    end.


-spec source_location(ReplState) -> Source | no_return()
    when ReplState :: aere_repl_state:state(),
         Source    :: aere_theme:renderable().

source_location(RS) ->
    ES = breakpoint_engine_state(RS),

    {FileName, CurrentLine} = aefa_engine_state:debugger_location(ES),

    File     = aere_utils:read_file(FileName),
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
        _                -> throw({repl_error, aere_msg:not_at_breakpoint()})
    end.
