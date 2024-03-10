-module(aere_debugger).

-include_lib("aebytecode/include/aeb_fate_data.hrl").

-export([ add_breakpoint/3
        , delete_breakpoint/2
        , delete_breakpoint/3
        , stop/1
        , resume_eval/2
        , lookup_variable/2
        , get_variables/1
        , source_location/1
        , stacktrace/1
        ]).

-type source_location() ::
        #{ file := string()
         , line := non_neg_integer()
         , preview_above := [binary()]
         , preview_line := binary()
         , preview_below := [binary()]
         }.

-export_type([source_location/0]).


-type break_state() ::
        #{ reason => breakpoint | abort
         , engine_state => aefa_engine_state:state()
         , init_trees => aec_trees:trees()
         }.

-export_type([break_state/0]).


-spec add_breakpoint(ReplState, FileName, Line) -> ReplState
    when ReplState :: aere_repl_state:state(),
         FileName  :: string(),
         Line      :: integer().

add_breakpoint(State, FileName, Line) ->
    LoadedFiles = maps:keys(aere_repl_state:loaded_files(State)),
    [ throw({repl_breakpoint_file_not_loaded, FileName})
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
    [ throw({repl_breakpoint_out_of_range, Index})
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
    [ throw({repl_breakpoint_wrong_location, File, Line})
        || length(BPs) == length(NewBPs) ],
    aere_repl_state:set_breakpoints(NewBPs, State).


-spec stop(ReplState) ->  ReplState | no_return()
    when ReplState   :: aere_repl_state:state().

stop(RS0) ->
    case get_break_state(RS0) of
        #{} ->
            RS1 = aere_repl_state:remove_user_input_file(RS0),
            aere_repl_state:restore_ready_state(RS1)
    end.


-spec resume_eval(ReplState, ResumeKind) -> {Result, ReplState} | no_return()
    when ReplState   :: aere_repl_state:state(),
         Result      :: {msg, aere_theme:renderable()}
                      | aere_fate:eval_debug_result(),
         ResumeKind  :: continue | stepin | stepout | stepover.

resume_eval(RS, Kind) ->
    case get_break_state(RS) of
        #{reason := abort} ->
            Trees = aere_repl_state:trees(RS),
            NewRS = aere_repl_state:set_ready_state(Trees, RS),
            {contract_exec_ended, NewRS};
        #{reason := breakpoint, engine_state := ES0} ->
            ES1 = resume(ES0, Kind),
            aere_fate:resume_contract_debug(ES1, RS)
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


-spec lookup_variable(ReplState, VariableName) -> string() | no_return()
    when ReplState    :: aere_repl_state:state(),
         VariableName :: term().

lookup_variable(RS, VarName) ->
    #{engine_state := ES} = get_break_state(RS),
    case aefa_debug:get_variable_register(VarName, aefa_engine_state:debug_info(ES)) of
        undefined ->
            throw({repl_undefined_variable, VarName});
        Reg ->
            {Val, _} = aefa_fate:lookup_var(Reg, ES),
            Val %% TODO: some fate type?
    end.


-spec get_variables(ReplState) -> Vars
    when ReplState  :: aere_repl_state:state(),
         Vars :: list({string(), string()}).

get_variables(RS) ->
    #{engine_state := ES} = get_break_state(RS),
    AllVars = aefa_debug:vars_registers(aefa_engine_state:debug_info(ES)),

    %% Filter out the variables with no mapped registers (variables defined in
    %% the repl instead of the called debugged code)
    VarsRegisters = maps:filter(fun(_, []) -> false; (_, _) -> true end, AllVars),

    Vars = [{K, lookup_variable(RS, K)} || K <- maps:keys(VarsRegisters)],
    Vars.


get_source_text(FileName, RS) ->
    LoadedFiles = aere_repl_state:loaded_files(RS),
    case maps:get(FileName, LoadedFiles, undefined) of
        undefined ->
            % Trust the chain
            #{engine_state := ES} = get_break_state(RS),
            case aefa_engine_state:current_contract(ES) of
                ?FATE_VOID ->
                    undefined;
                PubKey ->
                    Trees = aere_repl_state:trees(RS),
                    CTree = aec_trees:contracts(Trees),
                    case aect_state_tree:lookup_contract_with_code(PubKey, CTree) of
                        none ->
                            undefined;
                        {value, _Contract, Code} ->
                            case aeser_contract_code:deserialize(Code) of
                                %% TODO Does this ever match actually?
                                #{contract_source := Src} -> Src;
                                _Code ->
                                    undefined
                            end
                    end
            end;
        File -> File
    end.


split_src_lines(File, LocBackwards, CurrentLine, LocForwards) ->
    Lines        = string:split(File, "\n", all),
    SelectLines  = [ {Idx, Line}
                     || {Idx, Line} <- lists:enumerate(Lines),
                        Idx > CurrentLine - LocBackwards,
                        Idx < CurrentLine + LocForwards
                   ],

    ViewBackwards = [<<L/binary, "\n">> || {I, L} <- SelectLines, I < CurrentLine],
    ViewHere = hd([<<L/binary, "\n">> || {I, L} <- SelectLines, I == CurrentLine]),
    ViewForwards = [<<L/binary, "\n">> || {I, L} <- SelectLines, I > CurrentLine],
    {ViewBackwards, ViewHere, ViewForwards}.


-spec source_location(ReplState) -> Location | no_return()
    when ReplState :: aere_repl_state:state(),
         Location  :: source_location().

source_location(RS) ->
    #{engine_state := ES} = get_break_state(RS),
    Dbg = aefa_engine_state:debug_info(ES),
    {FileName, CurrentLine} = aefa_debug:debugger_location(Dbg),
    #{ loc_backwards := LocBackwards
     , loc_forwards := LocForwards
     } = aere_repl_state:options(RS),

    {ViewBackwards, ViewHere, ViewForwards} =
        case get_source_text(FileName, RS) of
            undefined ->
                {[], no_src, []};
            Src -> split_src_lines(
                     Src,
                     LocBackwards, CurrentLine, LocForwards
                    )
        end,

    #{file => FileName,
      line => CurrentLine
     , preview_above => ViewBackwards
     , preview_line => ViewHere
     , preview_below => ViewForwards
     }.


-spec stacktrace(ReplState) -> Message | no_return()
    when ReplState :: aere_repl_state:state(),
         Message   :: aere_fate:stacktrace().

stacktrace(RS) ->
    #{engine_state := ES} = get_break_state(RS),
    aere_fate:get_stack_trace(RS, ES).


-spec get_break_state(ReplState) -> EngineState | no_return()
    when ReplState   :: aere_repl_state:state(),
         EngineState :: aefa_engine_state:state().

get_break_state(RS) ->
    case aere_repl_state:blockchain_state(RS) of
        {break, BS} -> BS;
        {ready, _}  -> throw(repl_not_at_breakpoint)
    end.
