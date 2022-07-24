-module(aere_version).

-export([ protocol_version/0
        , abi_version/0
        , sophia_version/0
        , contract_version/0
        , vm_version/0
        , repl_version/0
        ]).

-include("../node/apps/aecontract/src/aect_sophia.hrl").
-include("../node/apps/aecontract/include/aecontract.hrl").
-include("../node/apps/aecontract/include/hard_forks.hrl").
-include("../node/apps/aecontract/test/include/aect_sophia_vsn.hrl").

protocol_version() -> ?IRIS_PROTOCOL_VSN.
sophia_version()   -> ?SOPHIA_IRIS_FATE.
abi_version()      -> ?ABI_FATE_SOPHIA_1.
vm_version()       -> ?VM_FATE_SOPHIA_2.
contract_version() -> ?SOPHIA_CONTRACT_VSN_3.
repl_version() ->
    case lists:keyfind(aerepl, 1, application:loaded_applications()) of
        false ->
            case application:load(aerepl) of
                ok ->
                    case application:get_key(aerepl, vsn) of
                        {ok, VsnString} ->
                            VsnString;
                        undefined ->
                            "???"
                    end;
                _ -> "???"
            end;
        {_App, _Des, VsnString} ->
            VsnString
    end.
