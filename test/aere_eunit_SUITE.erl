-module(aere_eunit_SUITE).

-compile([export_all, nowarn_export_all]).

all() ->
    [{group, eunit}].

groups() ->
    [{eunit, [], [ aere_tests
                 ]}].

aere_tests(_Config) ->
    ok = eunit:test(aere_tests).
