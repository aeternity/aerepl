-module(aere_debugger).

-export([ add_breakpoint/3
        , delete_breakpoint/2
        , list_breakpoints/1
        , resume/2
        , lookup_variable/2
        , source_location/1
        ]).

-spec add_breakpoint(FileName, Line, State) -> State
    when FileName :: string(),
         Line     :: integer(),
         State    :: aere_repl_state:state().
add_breakpoint(FileName, Line, State) ->
    OldBPs = aere_repl_state:breakpoints(State),
    BP     = {FileName, list_to_integer(Line)},
    NewBPs = OldBPs ++ [BP],
    aere_repl_state:set_breakpoints(NewBPs, State).

-spec delete_breakpoint(Index, State) -> State | no_return()
    when Index :: integer(),
         State :: aere_repl_state:state().
delete_breakpoint(Index, State) ->
    BPs = aere_repl_state:breakpoints(State),
    [ throw({repl_error, aere_msg:error("Breakpoint does not exist")})
        || Index < 1 orelse Index > length(BPs) ],
    {Left, [_ | Right]} = lists:split(Index - 1, BPs),
    aere_repl_state:set_breakpoints(Left ++ Right, State).

-spec list_breakpoints(State) -> string()
    when State :: aere_repl_state:state().
list_breakpoints(State) ->
    BPs  = aere_repl_state:breakpoints(State),
    Enum = fun(List) -> lists:zip(lists:seq(1, length(List)), List) end,
    Msg  = "~p    Breakpoint in the file '~s' at line ~p\n",
    lists:flatten([ io_lib:format(Msg, [I, F, L])  || {I, {F, L}} <- Enum(BPs) ]).

-spec resume(EngineState, ResumeKind) -> EngineState
    when EngineState :: aefa_engine_state:state(),
         ResumeKind  :: step | next | continue | finish.
resume(ES, Kind) ->
    CurFun = aefa_engine_state:current_function(ES),
    Status =
        case Kind of
            K when K == next; K == finish -> {K, CurFun};
            _                             -> Kind
        end,
    ES1 = aefa_engine_state:set_breakpoint_stop(false, ES),
    ES2 = aefa_engine_state:set_debugger_status(Status, ES1),
    ES2.

-spec lookup_variable(EngineState, VariableName) -> string() | no_return()
    when EngineState  :: aefa_engine_state:state(),
         VariableName :: string().
lookup_variable(ES, VarName) ->
    case aefa_engine_state:get_variable_register(VarName, ES) of
        undefined ->
            throw({repl_error, aere_msg:error("Undefined variable " ++ VarName)});
        Reg ->
            {Val, _} = aefa_fate:lookup_var(Reg, ES),
            io_lib:format("~p", [Val])
    end.

-spec source_location(EngineState) -> Source
    when EngineState :: aefa_engine_state:state(),
         Source      :: string().
source_location(ES) ->
    {FileName, CurrentLine} = aefa_engine_state:debugger_location(ES),

    {ok, File} = aere_utils:read_file(FileName),
    Lines      = string:split(File, "\n", all),
    LineSign   =
        fun(Id) when Id == CurrentLine -> ">";
           (_)                         -> "|"
        end,
    MaxDigits     = length(integer_to_list(length(Lines))),
    FormatLineNum = fun(Num) -> string:right(integer_to_list(Num), MaxDigits) end,
    FormatLine    = fun(N, Ln) -> [LineSign(N), " ", FormatLineNum(N), " ", Ln] end,
    Enumerate     = fun(List) -> lists:zip(lists:seq(1, length(List)), List) end,
    NewLines      = [ FormatLine(Idx, Line) || {Idx, Line} <- Enumerate(Lines) ],
    lists:join("\n", NewLines).
