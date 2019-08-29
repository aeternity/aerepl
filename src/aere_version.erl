-module(aere_version).

-export([latest_protocol_version/0, latest_sophia_abi_version/0, latest_sophia_version/0, latest_sophia_contract_version/0, latest_sophia_vm_version/0]).

-include("../apps/aecontract/src/aect_sophia.hrl").
-include("../apps/aecontract/include/aecontract.hrl").
-include_lib("../apps/aecontract/include/hard_forks.hrl").
-include("../apps/aecontract/test/include/aect_sophia_vsn.hrl").


latest_sophia_vm_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?VM_AEVM_SOPHIA_1;
        ?MINERVA_PROTOCOL_VSN -> ?VM_AEVM_SOPHIA_2;
        ?FORTUNA_PROTOCOL_VSN -> ?VM_AEVM_SOPHIA_3;
        ?LIMA_PROTOCOL_VSN    -> ?VM_AEVM_SOPHIA_4
    end.

latest_sophia_abi_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?ABI_AEVM_SOPHIA_1;
        ?MINERVA_PROTOCOL_VSN -> ?ABI_AEVM_SOPHIA_1;
        ?FORTUNA_PROTOCOL_VSN -> ?ABI_AEVM_SOPHIA_1;
        ?LIMA_PROTOCOL_VSN    -> ?ABI_AEVM_SOPHIA_1
    end.

latest_sophia_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?SOPHIA_ROMA;
        ?MINERVA_PROTOCOL_VSN -> ?SOPHIA_MINERVA;
        ?FORTUNA_PROTOCOL_VSN -> ?SOPHIA_FORTUNA;
        ?LIMA_PROTOCOL_VSN    -> ?SOPHIA_LIMA_AEVM
    end.

latest_sophia_contract_version() ->
    case latest_protocol_version() of
        ?ROMA_PROTOCOL_VSN    -> ?SOPHIA_CONTRACT_VSN_1;
        ?MINERVA_PROTOCOL_VSN -> ?SOPHIA_CONTRACT_VSN_2;
        ?FORTUNA_PROTOCOL_VSN -> ?SOPHIA_CONTRACT_VSN_2;
        ?LIMA_PROTOCOL_VSN    -> ?SOPHIA_CONTRACT_VSN_3
    end.

latest_protocol_version() ->
    lists:last(aec_hard_forks:sorted_protocol_versions()).
