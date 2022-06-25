-record(repl_options,
        { coloring :: coloring()
        , display_gas = true :: boolean()
        , call_gas = 1000000000000000000 :: pos_integer()
        }).
-type repl_options() :: #repl_options{}.

-type chain_state() :: {ready, aefa_chain_api:state()}
                     | {breakpoint, aefa_engine_state:state()}.

-record(repl_state,
        { blockchain_state :: chain_state()
        , repl_account :: binary()
        , options :: repl_options()
        , vars = [] :: [{string(), aeso_syntax:type(), term()}]
        , funs = #{} :: #{binary() => term()}
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
