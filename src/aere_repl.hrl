-define(DEFAULT_CONTRACT_STATE_T, {tuple_t, aere_mock:ann(), []}).
-define(DEFAULT_CONTRACT_STATE, {?DEFAULT_CONTRACT_STATE_T, {tuple, {}}}).

-record(repl_response,
        { output :: aere_theme:renderable()
        , warnings :: [string()]
        , status :: {ok, aere_repl_state:state()}
                  | error
                  | internal_error
                  | skip
                  | finish
        }).
-type repl_response() :: #repl_response{}.
