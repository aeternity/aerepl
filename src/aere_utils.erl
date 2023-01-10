-module(aere_utils).

-export([ read_file/1
        , read_files/1
        ]).

-spec read_files(FileNames) -> [{FileName, File}] | no_return()
    when FileNames :: [FileName],
         FileName  :: string(),
         File      :: binary().
read_files(Filenames) ->
    Files = [read_file(F) || F <- Filenames],

    case [{File, file:format_error(Err)} || {File, {error, Err}} <- lists:zip(Filenames, Files)] of
        []     -> ok;
        Failed -> throw({repl_error, aere_msg:files_load_error(Failed)})
    end,

    lists:zip(Filenames, [File || {ok, File} <- Files]).

%% Reads files either from working dir or stdlib
-spec read_file(FileName) -> {ok, binary()} | {error, term()}
    when FileName :: string().
read_file(Filename) ->
    case file:read_file(Filename) of
        {error, enoent} ->
            file:read_file(filename:join(aeso_stdlib:stdlib_include_path(), Filename));
        Res -> Res
    end.
