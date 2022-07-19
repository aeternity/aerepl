-module(aere_chain).

-include("../node/apps/aecontract/include/aecontract.hrl").
-include("../node/apps/aecore/include/blocks.hrl").

-export([init_trees/0, new_account/2, update_balance/3, default_tx_env/1]).


init_trees() ->
    aec_trees:new().

new_account(Balance, Trees) ->
    #{public := PubKey} = enacl:sign_keypair(),
    Trees1      = set_account(aec_accounts:new(PubKey, Balance), Trees),
    {PubKey, Trees1}.

update_balance(NewBalance, PubKey, Trees) ->
    Account = aec_accounts_trees:get(PubKey, aec_trees:accounts(Trees)),
    OldBalance = aec_accounts:balance(Account),
    Nonce = aec_accounts:nonce(Account),
    {ok, Account1} = aec_accounts:spend(Account, OldBalance, Nonce + 1),
    {ok, Account2} = aec_accounts:earn(Account1, NewBalance),
    set_account(Account2, Trees).

set_account(Account, Trees) ->
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    aec_trees:set_accounts(Trees, AccTree).

-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).
default_tx_env(Height) ->
    Env = aetx_env:set_beneficiary(aetx_env:tx_env(Height), ?BENEFICIARY_PUBKEY),
    aetx_env:set_dry_run(Env, true).
