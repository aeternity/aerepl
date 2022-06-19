-record(repl_state,
        { blockchain_state :: aefa_chain_api:state() %% blockchain
        , repl_account :: binary()
        }).
-type repl_state() :: repl_state().

-record(repl_response,
        { output :: string()
        , warnings :: [string()]
        , status :: {ok, repl_state()}
                  | error
                  | internal_error
                  | finish
        }).
-type repl_response() :: repl_response().

-type color() :: default
               | emph
               | black
               | red
               | green
               | yellow
               | blue
               | magenta
               | cyan
               | white
               .

-type colored()
 :: string()
  | {colored, color(), string()}
  | list(colored()).
