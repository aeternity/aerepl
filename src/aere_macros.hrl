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

-define(LAZY(C), fun() -> C end).
-define(IF(C, T, E),
        case C of
            true -> T;
            false -> E
            %% _ -> error(condition_not_a_bool)
        end).
