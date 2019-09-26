-module(aere_mock).

-include("aere_repl.hrl").

-export([ ann/0, contract/1, contract/2, typedef/2, entrypoint/3, entrypoint/2
        , decl/3
        ]).

ann() ->
    [{file, <<"REPL">>}].
contract(Body) ->
    contract(?MOCK_CONTRACT, Body).
contract(Name, Body) ->
    {contract, ann(), {con, ann(), Name},
     Body
    }.
typedef(Name, Type) ->
    {type_def, ann(),
     {id, ann(), Name},
     [],
     {alias_t, Type}}.
entrypoint(Name, Body) ->
    entrypoint(Name, Body, []).
entrypoint(Name, Body, full) ->
    entrypoint(Name, Body, [payable, stateful]);
entrypoint(Name, Body, Attrs) when is_list(Attrs) ->
    { letfun
    , ann() ++ [{A, true} || A <- [entrypoint|Attrs]]
    , {id, ann(), Name}
    , []
    , {id, ann(), "_"}
    , Body}.
decl(Name, Type, Attrs) ->
    { fun_decl
    , ann() ++ [{A, true} || A <- Attrs]
    , {id, [], Name}
    , {fun_t, [], [], [], Type}
    }.
