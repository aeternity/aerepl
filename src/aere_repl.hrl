-record(options,
        { display_call_gas :: boolean()
        , display_deploy_gas :: boolean()
        , gas :: non_neg_integer()
        , height :: non_neg_integer()
        , call_value :: non_neg_integer()
        , colors :: none | default | no_emph
        , silent :: boolean()
        , display_unit :: boolean()
        }).
-type options() :: options().

-record(repl_state,
        { include_ast :: list(aeso_ast:ast())
        , include_hashes :: sets:set(aeso_parser:include_hash())
        , include_files :: list(string())
        , options :: options()
        , chain_state %% blockchain
        , user_contract_state_type %% type of the contract `state`
        , user_contracts %% contracts that were used to perform user calls
        , tracked_contracts
          %% :: [{ <variable name>
          %%       { tracked_contract | shadowed_contract
          %%       , <contract address>
          %%       , <ACI>
          %%       }}]
        , letfuns
          %% :: [{ <function name>
          %%       { <declarations>
          %%       , <used tracked contracts>
          %%       , <used letvals>
          %%       }
          %%     }]
        , letvals
          %% :: [{ {<letval provider name>, <letval provider address>}
          %%     , {<pattern>, <typedef aliases>}
          %%     }]
        , typedefs
          %% :: [{ <type name>
          %%     , {<args>, <typedef>}
          %%     }]
        , type_alias_map
          %% :: [{<type name>, {typedef, <args>, <namespace>, <definition>}
          %%                 | {contract, <internal name>}}
          %%                 | {constructor, <namespace>}]
        , user_account
        , supply :: integer()
        , cwd :: string()
        , warnings
        }).
-type repl_state() :: repl_state().

-record(repl_response,
        { output :: string()
        , warnings :: [string()]
        , status :: {success, repl_state()}
                  | ask
                  | error
                  | internal_error
                  | finito
        }).
-type repl_response() :: repl_response().

-record(repl_question,
        { text :: string()
        , options :: {string(), function()}
        , default :: string()
        , callback :: function()
        }).
-type repl_question() :: repl_question().
