-module(aere_error).
-compile([export_all, nowarn_export_all]).

internal(Error, Stacktrace) ->
    [ "Command failed:\n"
    , aere_color:error(lists:flatten(io_lib:format("~p", [Error]))), "\n"
    , case Stacktrace of
          [] -> "<no stacktrace>";
          _ -> ["Stacktrace:\n", io_lib:format("~p", [Stacktrace]), "\n\n"]
      end
    ].

internal(Error) ->
    internal(Error, []).

no_such_command(Command) ->
    ["No such command ", aere_color:command(io_lib:format("~p", [Command])), "\n"].

ambiguous_prefix(Propositions) ->
    PropsString =
        aere_color:command(lists:flatten([io_lib:format(" ~p", [P]) || P <- Propositions])),
    ["Ambiguous command prefix. Matched commands: ", PropsString, "\n"].

bad_option(Expected) ->
    {error,
     [aere_color:error(case Expected of
                            [] -> "nothing";
                            [X] -> X;
                            [E|More] -> [E|[[", ", M] || M <- More]]
                        end), " expected"]}.

unknown_option(Prop) ->
    {error,
     ["Unknown setting ", aere_color:setting(Prop)]}.

forbidden_id(Name) ->
    {error,
     ["\"", Name, "\" is forbidden here"]}.

unsupported_decl(multidecl) ->
    {error,
     "One by one please"};
unsupported_decl(type_decl) ->
    {error,
     "Type declaration is not yet supported"};
unsupported_decl(type_def) ->
    {error,
     "Type definition is not yet supported"};
unsupported_decl(contract) ->
    {error,
     ["Contracts cannot be defined here}). Please consider ", aere_color:command(":deploy")]};
unsupported_decl(namespace) ->
    {error,
     "Namespaces can't be defined here"}.

uninclude_error(Er) ->
    {error,
     ["Removing includes will cause an error. Please remove conflicting entities first;\n", Er]}.

no_file_deploy() ->
    {error,
     "I need a file"}.
parse_deploy() ->
    {error,
     [ "Bad format. Valid formats are ", aere_color:command(":deploy [FILE NAME]")
     , " and ", aere_color:command(":deploy [FILE NAME] as [VARIABLE NAME]")
     ]}.
bad_deploy_name() ->
    {error,
     "Contract name must begin with a lowercase letter and contain only letters, numbers or underscores"}.

file_error(File, Reason) ->
    {error,
     case Reason of
         enoent -> ["No such file or directory ", aere_color:file(File)];
         eaccess -> "Permission denied";
         eisdir -> [aere_color:file(File), " is a directory"];
         enotdir -> [aere_color:file(File), "is not a directory"];
         enomem -> [aere_color:file(File), " is too big"];
         _ -> "Unknown error"
     end}.


nothing_to_remove() ->
    {error, ["Nothing to remove"]}.


contract_creation_error(Reason) ->
    {error,
     ["Failed to create contract: ", Reason]}.


locked_cwd() ->
    { error
    , "Changing working dir is forbidden"
    }.


undefined_command() ->
    {error,
     "Uhm, this command seems to be undefined while it should be"}.

