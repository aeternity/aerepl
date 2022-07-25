-module(aere_chain).

-include("../node/apps/aecore/include/blocks.hrl").

-export([ new_account/2
        , default_tx_env/1
        ]).

-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).

-spec new_account(Balance, Trees) -> {aec_keys:pubkey(), Trees} when
      Balance :: non_neg_integer(),
      Trees :: aec_trees:trees().
new_account(Balance, Trees) ->
    #{public := PubKey} = enacl:sign_keypair(),
    Trees1      = set_account(aec_accounts:new(PubKey, Balance), Trees),
    {PubKey, Trees1}.

-spec set_account(aec_accounts:account(), Trees) -> Trees when
      Trees :: aec_trees:trees().
set_account(Account, Trees) ->
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    aec_trees:set_accounts(Trees, AccTree).

-spec default_tx_env(aec_blocks:height()) -> aetx_env:env().
default_tx_env(Height) ->
    Env = aetx_env:set_beneficiary(aetx_env:tx_env(Height), ?BENEFICIARY_PUBKEY),
    aetx_env:set_dry_run(Env, true).
