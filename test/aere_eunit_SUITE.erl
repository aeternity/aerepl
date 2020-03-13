-module(aere_eunit_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, eunit}].

groups() ->
    [{eunit, [], [ aere_tests
                 ]}].

aere_tests(_Config) ->
    io:format("HELLO"),
    ok = eunit:test(aere_tests).
