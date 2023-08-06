-module(aere_files).

-export([ read_file/2
        , read_files/2
        ]).

-type repl_state() :: aere_repl_state:state().
-type filename() :: string().
-type file_contents() :: binary().

-spec read_file(filename(), repl_state()) -> file_contents() | no_return().

read_file(FileName, S) ->
    case read_file_safe(FileName, S) of
        {ok, File} ->
            File;
        {error, Err} ->
            Failed = [{FileName, file:format_error(Err)}],
            throw({repl_error, aere_msg:files_load_error(Failed)})
    end.


-spec read_files([filename()], repl_state()) -> [{filename(), file_contents()}] | no_return().

read_files(FileNames, S) ->
    Files  = [read_file_safe(F, S) || F <- FileNames],
    Failed = [{File, file:format_error(Err)} || {File, {error, Err}} <- lists:zip(FileNames, Files)],
    case Failed of
        [] -> lists:zip(FileNames, [File || {ok, File} <- Files]);
        _  -> throw({repl_error, aere_msg:files_load_error(Failed)})
    end.


-spec read_file_safe(filename(), repl_state()) -> {ok, file_contents()} | {error, term()}.
%% @doc
%% Dispatches the source of the filesystem depending on the state and availability, and reads
%% the file.
read_file_safe(FileName, S) ->
    case aere_repl_state:filesystem(S) of
        local -> read_file_local(FileName);
        {cached, Fs} -> read_file_cached(FileName, Fs)
    end.


-spec read_file_local(FileName) -> {ok, file_contents()} | {error, term()}
    when FileName :: filename().
%% @doc
%% Reads files either from working dir or stdlib.

read_file_local(FileName) ->
    case file:read_file(FileName) of
        {error, enoent} ->
            read_file_stdlib(FileName);
        Res ->
            {ok, Res}
    end.


-spec read_file_cached(FileName, Files) -> {ok, file_contents()} | {error, term()}
    when FileName :: filename(),
         Files    :: #{filename() => file_contents()}.
%% @doc
%% Reads files either from filesystem cache or stdlib.

read_file_cached(FileName, Files) ->
    case maps:get(FileName, Files, undefined) of
        undefined ->
            read_file_stdlib(FileName);
        Res ->
            {ok, Res}
    end.

-spec read_file_stdlib(FileName) -> {ok, file_contents()} | {error, term()}
    when FileName :: filename().
%% @doc
%% Reads files from stdlib.

read_file_stdlib(FileName) ->
    StdLibPath = aeso_stdlib:stdlib_include_path(),
    FilePath = filename:join(StdLibPath, FileName),
    file:read_file(FilePath).
