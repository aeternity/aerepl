#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -setcookie aerepl_cookie -mnesia debug verbose
-mode(compile).

main(_) ->
  % dont judge me
  Name = list_to_atom(lists:filter(fun(X) -> (X >= $0) and ($9 >= X) end, lists:flatten(io_lib:format("~p", [make_ref()])))),
  net_kernel:start([Name, shortnames]),
  aere_cli:run().