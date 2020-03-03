-module(enacl_nif_bak).

-export([load/1]).


load(Path) ->
    erlang:load_nif(Path ++ "/enacl/priv/enacl_nif", 0).
