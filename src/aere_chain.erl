-module(aere_chain).

-include("../node/apps/aecore/include/blocks.hrl").

-export([ new_keypair/0
        , new_account/2
        , set_account/3
        , default_call_origin/0
        , default_call_beneficiary/0
        , get_all_accounts/1
        , default_tx_env/2
        ]).

default_call_origin() ->
    <<0:256>>.

default_call_beneficiary() ->
    <<0:?BENEFICIARY_PUB_BYTES/unit:8>>.

new_keypair() ->
    enacl:sign_keypair().

-spec new_account(Balance, Trees) ->
          #{ public := aec_keys:pubkey()
           , private := aec_keys:privkey()
           , trees := Trees
           } when
      Balance :: non_neg_integer(),
      Trees :: aec_trees:trees().
new_account(Balance, Trees) ->
    #{public := PubKey, secret := PrivKey} = new_keypair(),
    Trees1      = set_account(PubKey, Balance, Trees),
    #{ public  => PubKey
     , private => PrivKey
     , trees   => Trees1
     }.

-spec set_account(aec_keys:pubkey(), non_neg_integer(), Trees) -> Trees when
      Trees :: aec_trees:trees().
set_account(PubKey, Balance, Trees) ->
    set_account(aec_accounts:new(PubKey, Balance), Trees).

-spec set_account(aec_accounts:account(), Trees) -> Trees when
      Trees :: aec_trees:trees().
set_account(Account, Trees) ->
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    aec_trees:set_accounts(Trees, AccTree).

-spec default_tx_env(aec_blocks:height(), aec_keys:pubkey()) -> aetx_env:env().
default_tx_env(Height, Beneficiary) ->
    Env = aetx_env:set_beneficiary(aetx_env:tx_env(Height), Beneficiary),
    aetx_env:set_dry_run(Env, true).

-spec get_all_accounts(Trees) -> list({PK, Balance}) when
      Trees :: aec_trees:trees(),
      PK :: aec_keys:pubkey(),
      Balance :: non_neg_integer().
get_all_accounts(Trees) ->
    Accounts = aec_trees:accounts(Trees),
    aec_accounts:get_all_accounts_balances(Accounts).
