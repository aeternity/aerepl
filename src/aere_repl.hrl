-record(repl_options,
        { theme :: aere_theme:theme()
        , display_gas = true :: boolean()
        , call_gas = 1000000000000000000 :: pos_integer()
        }).
-type repl_options() :: #repl_options{}.

-type chain_state() :: {ready, aefa_chain_api:state()}
                     | {breakpoint, aefa_engine_state:state()}.

-record(repl_state,
        { blockchain_state     :: chain_state()
        , repl_account         :: binary()
        , options              :: repl_options()
        , vars        = []     :: [{string(), aeso_syntax:type(), term()}]
        , funs        = #{}    :: #{binary() => term()}
        , typedefs    = []     :: [{string(), string(), [aeso_syntax:tvar()], aeso_syntax:typedef()}]
        , type_scope  = []     :: [{string(), {string(), non_neg_integer()}}]
        , loaded_files = #{}   :: #{string() => binary()} % Loaded files ready to be included
        , included_files = []  :: [string()] % Files included in the context
        , included_code = []   :: aeso_syntax:ast() % Cached AST of the included files
        , query_nonce = 0      :: non_neg_integer()
        }).
-type repl_state() :: #repl_state{}.

-record(repl_response,
        { output :: aere_theme:renderable()
        , warnings :: [string()]
        , status :: {ok, repl_state()}
                  | error
                  | internal_error
                  | skip
                  | finish
        }).
-type repl_response() :: #repl_response{}.

-type command_res() :: finish | {colored(), repl_state()} | repl_state() | none().

-type color() :: atom() | {[atom()], atom()}.

-type colored() :: string() | {colored, color(), string()} | list(colored()).
