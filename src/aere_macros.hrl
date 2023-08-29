%% These names are not supposed to be accepted by the parser
-define(USER_INPUT, "$REPL_user_input").
-define(MOCK_CONTRACT, "$REPL_contract").
-define(TYPE_CONTAINER, "$REPL_type").
-define(TYPE_CONTAINER(T), ?TYPE_CONTAINER ++ integer_to_list(T)).
-define(TYPE_CONTAINER_RE, "\\" ++ ?TYPE_CONTAINER ++ "[0-9]*\\.").

-define(IS_REPL_ENTRYPOINT(X), lists:member(X, [<<?USER_INPUT>>, <<"init">>])).

-define(DEFAULT_CONTRACT_STATE_T, {tuple_t, aere_mock:ann(), []}).
-define(DEFAULT_CONTRACT_STATE, {?DEFAULT_CONTRACT_STATE_T, {tuple, {}}}).

-define(LAZY(C), fun() -> C end).
-define(IF(C, T, E),
        case C of
            true -> T;
            false -> E
        end).
