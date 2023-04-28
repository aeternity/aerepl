-module(aere_files).

-export([ read_file/1
        , read_files/1
        ]).


-spec read_file(FileName) -> File | no_return()
    when FileName :: string(),
         File     :: binary().

read_file(FileName) ->
    case read_file_safe(FileName) of
        {ok, File} ->
            File;
        {error, Err} ->
            Failed = [{FileName, file:format_error(Err)}],
            throw({repl_error, aere_msg:files_load_error(Failed)})
    end.


-spec read_files([FileName]) -> [{FileName, File}] | no_return()
    when FileName  :: string(),
         File      :: binary().

read_files(Filenames) ->
    Files  = [read_file_safe(F) || F <- Filenames],
    Failed = [{File, file:format_error(Err)} || {File, {error, Err}} <- lists:zip(Filenames, Files)],
    case Failed of
        [] -> lists:zip(Filenames, [File || {ok, File} <- Files]);
        _  -> throw({repl_error, aere_msg:files_load_error(Failed)})
    end.


-spec read_file_safe(FileName) -> {ok, binary()} | {error, term()}
    when FileName :: string().
%% @doc
%% Reads files either from working dir or stdlib.

read_file_safe(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            file:read_file(filename:join(aeso_stdlib:stdlib_include_path(), Filename));
        Res ->
            Res
    end.
