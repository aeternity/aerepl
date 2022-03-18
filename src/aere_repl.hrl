-record(options,
        { display_call_gas :: boolean()
        , display_deploy_gas :: boolean()
        , gas :: non_neg_integer()
        , height :: non_neg_integer()
        , call_value :: non_neg_integer()
        , colors :: none | default | no_emph
        , silent :: boolean()
        , display_unit :: boolean()
        , lock_cwd :: boolean()
        }).
-type options() :: options().

-record(repl_state,
        { include_ast :: list(aeso_syntax:ast())
        , include_hashes :: sets:set(aeso_parser:include_hash())
        , include_files :: list(string())
        , options :: options()
        , blockchain_state :: any() %% blockchain
        , user_contract_state_type :: aeso_syntax:type()
        , last_state_provider :: binary()
        , tracked_contracts :: list(tracked_contract())
        , defined_contracts :: list(defined_contract())
        , letfuns :: list(letfun())
        , letvals :: list(letval())
        , typedefs :: list(typedef())
        , type_alias_map :: list(type_alias())
        , user_account :: binary()
        , supply :: integer()
        , cwd :: string()
        , warnings :: [string()]
        }).
-type repl_state() :: repl_state().

-type letfun() ::
        { string() %% name
        , { list(aeso_syntax:decl()) %% clauses
          , list(tracked_contract()) %% scope of contracts
          , list(letval()) %% scope of letvals
          }}.
-type tracked_contract() ::
        { string()
        , { tracked_contract | shadowed_contract
          , aeso_syntax:constant() %% Reference address
          , aeso_syntax:decl() %% ACI
          }}.
-type defined_contract() :: aeso_syntax:decl().
-type letval() ::
        { { string() %% name
          , aeso_syntax:constant() %% provider address
          }
        , { aeso_syntax:pat() %% pattern of definition
          , list(type_alias()) %% used types at the moment of creation
          }}.
-type typedef() ::
        { aeso_syntax:id() | aeso_syntax:qid() %% identifier
        , { list(aeso_syntax:tvar()) %% args
          , aeso_syntax:typedef() %% definition
          }}.
-type type_alias() ::
        { string() %% name
          %% for type definitions
        , {typedef, list(aeso_syntax:tvar()), aeso_syntax:con(), aes}
          %% for tracked contracts
          | {contract, string()}
          %% for constructors
          | {constructor, aeso_syntax:con()}
        }.

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
        , callback :: fun((repl_state()) -> repl_state())
        }).
-type repl_question() :: repl_question().

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

-type colored() :: string()
                 | {colored, color(), integer(), colored()}
                 | list(colored()).
