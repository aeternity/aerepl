
-record(options,
        { display_call_gas :: boolean()
        , display_deploy_gas :: boolean()
        , gas :: non_neg_integer()
        , height :: non_neg_integer()
        , call_value :: non_neg_integer()
        , backend :: fate | aevm
        }).
-type options() :: options().

-record(repl_state,
        { include_ast :: list(aeso_ast:ast())
        , include_hashes :: sets:set(aeso_parser:include_hash())
        , include_files :: list(string())
        , options :: options()
        , chain_state
        , user_contract_state_type
        , user_contracts
        , tracked_contracts
        , let_defs
        }).

-type repl_state() :: repl_state().

-define(USER_INPUT, "__user_input__").
-define(GET_STATE, "__get_state__").
-define(MOCK_CONTRACT, <<"__mock_contract__">>).
-define(PREV_CONTRACT, <<"__prev_contract__">>).
