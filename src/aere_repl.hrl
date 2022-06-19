-record(repl_options,
        { coloring :: coloring()
        }).
-type repl_options() :: #repl_options{}.

-record(repl_state,
        { blockchain_state :: aefa_chain_api:state()
        , repl_account :: binary()
        , options :: repl_options()
        }).
-type repl_state() :: #repl_state{}.

-record(repl_response,
        { output :: string()
        , warnings :: [string()]
        , status :: {ok, repl_state()}
                  | error
                  | internal_error
                  | finish
        }).
-type repl_response() :: #repl_response{}.

-type command_res() :: finish | {colored(), repl_state()} | repl_state() | none().

-type color() :: atom() | {[atom()], atom()}.

-type coloring() :: #{atom() => color()}.

-type colored()
 :: string()
  | {colored, color(), string()}
  | list(colored()).
