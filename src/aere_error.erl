-module(aere_error).
-compile([export_all, nowarn_export_all]).

internal(Command, Error, Stacktrace) ->
    [ "Command ", aere_color:blue(lists:flatten(io_lib:format("~p", [Command]))), " failed:\n"
    , aere_color:red(lists:flatten(io_lib:format("~p", [Error]))), "\n"
    , case Stacktrace of
          [] -> "<no stacktrace>";
          _ -> ["Stacktrace:\n", io_lib:format("~p", [Stacktrace]), "\n\n"]
      end
    , aere_color:red("*** This is an internal error and most likely a bug.\n")
    ].

internal(Command, Error) ->
    internal(Command, Error, []).

no_such_command(Command) ->
    ["No such command ", aere_color:blue(io_lib:format("~p", [Command])), "\n"].

ambiguous_prefix(Propositions) ->
    PropsString =
        aere_color:blue(lists:flatten([io_lib:format(" ~p", [P]) || P <- Propositions])),
    ["Ambiguous command prefix. Matched commands: ", PropsString, "\n"].

bad_option(Expected) ->
    {error,
     [aere_color:yellow(case Expected of
                            [] -> "nothing";
                            [X] -> X;
                            [E|More] ->
                                [E|[[", ", M] || M <- More]]
                        end), " expected"]}.

unknown_option(Prop) ->
    {error,
     ["Unknown property ", aere_color:yellow(Prop)]}.

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
     ["Contracts cannot be defined here}). Please consider ", aere_color:blue(":deploy")]};
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
     [ "Bad format. Valid formats are ", aere_color:magenta(":deploy [FILE NAME]")
     , " and ", aere_color:magenta(":deploy [FILE NAME] as [VARIABLE NAME]")
     ]}.
bad_deploy_name() ->
    {error,
     "Contract name must begin with a lowercase letter and contain only letters, numbers or underscores"}.

file_error(File, Reason) ->
    {error,
     case Reason of
         enoent -> ["No such file ", aere_color:yellow(File)];
         eaccess -> "Permission denied";
         eisdir -> [aere_color:yellow(File), " is a directory"];
         enotdir -> "Invalid path";
         enomem -> [aere_color:yellow(File), " is too big"];
         _ -> "Unknown error"
     end}.


nothing_to_remove() ->
    {error, ["Nothing to remove"]}.


contract_creation_error(Reason) ->
    {error,
     ["Failed to create contract: ", Reason]}.

undefined_command() ->
    {error,
     "Uhm, this command seems to be undefined while it should be"}.
