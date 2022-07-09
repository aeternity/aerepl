-module(aere_error).

-export([ internal/2
        , internal/1
        , no_such_command/1
        , undefined_command/0
        ]).

-spec internal(term(), erlang:stacktrace()) -> aere_theme:renderable().
internal(Error, Stacktrace) ->
    [ aere_theme:output("Command failed:\n")
    , aere_theme:error(lists:flatten(io_lib:format("~p", [Error])))
    , aere_theme:output("\n")
    , case Stacktrace of
          [] -> aere_theme:output("<no stacktrace>");
          _  -> aere_theme:output("Stacktrace:\n" ++ io_lib:format("~p", [Stacktrace]))
      end
    ].

-spec internal(term()) -> aere_theme:renderable().
internal(Error) ->
    internal(Error, []).

-spec no_such_command(aere_repl:command()) -> aere_theme:renderable().
no_such_command(Command) ->
    [ aere_theme:output("No such command ")
    , aere_theme:command(io_lib:format("~p", [Command]))
    , aere_theme:output("\n")
    ].

-spec undefined_command() -> {error, string()}.
undefined_command() ->
    {error, "Uhm, this command seems to be undefined while it should be"}.

%bad_option(Expected) ->
%    {error,
%     [aere_theme:error(case Expected of
%                            [] -> "nothing";
%                            [X] -> X;
%                            [E|More] -> [E|[[", ", M] || M <- More]]
%                        end), " expected"]}.
%
%unknown_option(Prop) ->
%    {error,
%     ["Unknown setting ", aere_theme:setting(Prop)]}.
%
%forbidden_id(Name) ->
%    {error,
%     ["\"", Name, "\" is forbidden here"]}.
%
%unsupported_decl(multidecl) ->
%    {error,
%     "One by one please"};
%unsupported_decl(type_decl) ->
%    {error,
%     "Type declaration is not yet supported"};
%unsupported_decl(type_def) ->
%    {error,
%     "Type definition is not yet supported"};
%unsupported_decl(contract) ->
%    {error,
%     ["Contracts cannot be defined here}). Please consider ", aere_theme:command(":deploy")]};
%unsupported_decl(namespace) ->
%    {error,
%     "Namespaces can't be defined here"}.
%
%uninclude_error(Er) ->
%    {error,
%     ["Removing includes will cause an error. Please remove conflicting entities first;\n", Er]}.
%
%no_file_deploy() ->
%    {error,
%     "I need a file"}.
%parse_deploy() ->
%    {error,
%     [ "Bad format. Valid formats are ", aere_theme:command(":deploy [FILE NAME]")
%     , " and ", aere_theme:command(":deploy [FILE NAME] as [VARIABLE NAME]")
%     ]}.
%bad_deploy_name() ->
%    {error,
%     "Contract name must begin with a lowercase letter and contain only letters, numbers or underscores"}.
%
%file_error(File, Reason) ->
%    {error,
%     case Reason of
%         enoent -> ["No such file or directory ", aere_theme:file(File)];
%         eaccess -> "Permission denied";
%         eisdir -> [aere_theme:file(File), " is a directory"];
%         enotdir -> [aere_theme:file(File), "is not a directory"];
%         enomem -> [aere_theme:file(File), " is too big"];
%         _ -> "Unknown error"
%     end}.
%
%
%nothing_to_remove() ->
%    {error, ["Nothing to remove"]}.
%
%
%contract_creation_error(Reason) ->
%    {error,
%     ["Failed to create contract: ", Reason]}.
%
%
%locked_cwd() ->
%    { error
%    , "Changing working dir is forbidden"
%    }.
