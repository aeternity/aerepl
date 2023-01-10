-module(aere_repl_response).

-export([ new/2 ]).

-export([ output/1
        , status/1
        ]).

-type response_status() :: {ok, aere_repl_state:state()}
                         | error
                         | internal_error
                         | skip
                         | finish.

-record(resp, { output :: aere_theme:renderable()
              , status :: response_status()
              }).
-opaque response() :: #resp{}.

-export_type([response/0]).

-spec new(aere_theme:renderable(), response_status()) -> response().
new(Output, Status) ->
    #resp{output = Output, status = Status}.

-spec output(response()) -> aere_theme:renderable().
output(#resp{output = Output}) ->
    Output.

-spec status(response()) -> response_status().
status(#resp{status = Status}) ->
    Status.
