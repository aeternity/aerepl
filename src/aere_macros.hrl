%% These names are not supposed to be accepted by the parser
-define(USER_INPUT, "#user_input#INTERNAL_REPL").
-define(MOCK_CONTRACT, "#mock_contract#INTERNAL_REPL").
-define(LETVAL_INDICATOR, "#VALUES#INTERNAL_REPL").

-define(LAZY(C), fun() -> C end).
-define(IF(C, T, E),
        case C of
            true -> T;
            false -> E;
            _ -> error(condition_not_a_bool)
        end).
