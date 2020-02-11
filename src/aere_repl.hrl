
-record(options,
        { display_call_gas :: boolean()
        , display_deploy_gas :: boolean()
        , gas :: non_neg_integer()
        , height :: non_neg_integer()
        , call_value :: non_neg_integer()
        , backend :: fate | aevm
        , silent :: boolean()
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
        , letvals
        , letfuns
        , supply :: integer()
        }).

-type repl_state() :: repl_state().

%% These names are not supposed to be accepted by the parser
-define(USER_INPUT, "#user_input#INTERNAL_REPL").
-define(GET_STATE, "#get_state#INTERNAL_REPL").
-define(MOCK_CONTRACT, "#mock_contract#INTERNAL_REPL").
-define(PREV_CONTRACT, "#prev_contract#INTERNAL_REPL").
-define(LETVAL_PROVIDER(X), "#letdef_prov#" ++ X ++ "#INTERNAL_REPL").
-define(LETVAL_PROVIDER_DECL(X), "#letdef_prov_decl#" ++ X ++ "#INTERNAL_REPL").
-define(LETVAL_GETTER(X), "#val_get#" ++ X ++ "#INTERNAL_REPL").
-define(ADD_OWNER(Owner, X), X ++ "#for#" ++ Owner).
-define(TrackedContractName(Ref, TypeName),
        "#contract#" ++ Ref ++ "#" ++ TypeName ++ "#INTERNAL_REPL").
