-module(aere_version).

-export([ protocol_version/0
        , abi_version/1
        , sophia_version/1
        , contract_version/0
        , vm_version/1]).

-include("../node/apps/aecontract/src/aect_sophia.hrl").
-include("../node/apps/aecontract/include/aecontract.hrl").
-include("../node/apps/aecontract/include/hard_forks.hrl").
-include("../node/apps/aecontract/test/include/aect_sophia_vsn.hrl").


protocol_version() ->
    ?LIMA_PROTOCOL_VSN.

sophia_version(fate) ->
    ?SOPHIA_LIMA_FATE;
sophia_version(aevm) ->
    ?SOPHIA_LIMA_AEVM.

abi_version(fate) ->
    ?ABI_FATE_SOPHIA_1;
abi_version(aevm) ->
    ?ABI_AEVM_SOPHIA_1.

vm_version(fate) ->
    ?VM_FATE_SOPHIA_1;
vm_version(aevm) ->
    ?VM_AEVM_SOPHIA_4.

contract_version() ->
    ?SOPHIA_CONTRACT_VSN_3.
