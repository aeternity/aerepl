-type print_format() :: sophia | fate | json.

-type repl_options() ::
        #{ theme       := aere_theme:theme()
        , display_gas  := boolean()
        , call_gas     := pos_integer()
        , call_value   := non_neg_integer()
        , print_format := print_format()
        , print_unit   := boolean()
        , locked_opts  => [atom()]
        }.

-type chain_state() :: {ready, aefa_chain_api:state()}
                     | {breakpoint, aefa_engine_state:state()}.

-type contract_state() :: {aeso_syntax:type(), aeb_fate_data:fate_type()}.

-type var() :: {string(), aeso_syntax:type(), term()}.

-type type_def() :: {string(), string(), [aeso_syntax:tvar()], aeso_syntax:typedef()}.

-type type_scope() :: {string(), {string(), non_neg_integer()}}.

-define(DEFAULT_CONTRACT_STATE_T, {tuple_t, aere_mock:ann(), []}).
-define(DEFAULT_CONTRACT_STATE, {?DEFAULT_CONTRACT_STATE_T, {tuple, {}}}).

-record(repl_state,
        { blockchain_state     :: chain_state()
        , repl_account         :: binary()
        , options              :: repl_options()
        , contract_state       :: contract_state()
        , vars        = []     :: [var()]
        , funs        = #{}    :: #{binary() => term()}
        , typedefs    = []     :: [type_def()]
        , type_scope  = []     :: [type_scope()]
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

-type command_res() :: finish | {aere_theme:renderable(), repl_state()} | repl_state() | no_return().

