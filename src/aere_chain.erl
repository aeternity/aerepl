-module(aere_chain).

-include("../node/apps/aecontract/include/aecontract.hrl").
-include("../node/apps/aecore/include/blocks.hrl").

-export([new_account/2, default_tx_env/1, update_balance/3]).

new_account(Balance, Trees1) ->
    {PubKey, PrivKey} = new_key_pair(),
    Trees2            = set_account(aec_accounts:new(PubKey, Balance), Trees1),
    {PubKey, Trees2}.

update_balance(NewBalance, PubKey, State) ->
    Trees = trees(State),
    Account = aec_accounts_trees:get(PubKey, aec_trees:accounts(Trees)),
    OldBalance = aec_accounts:balance(Account),
    Nonce = aec_accounts:nonce(Account),
    {ok, Account1} = aec_accounts:spend(Account, OldBalance, Nonce + 1),
    {ok, Account2} = aec_accounts:earn(Account1, NewBalance),
    set_account(Account2, State).

new_key_pair() ->
    #{ public := PubKey, secret := PrivKey } = enacl:sign_keypair(),
    {PubKey, PrivKey}.

insert_key_pair(Pub, Priv, S) ->
    Old = key_pairs(S),
    S#{key_pairs => Old#{Pub => Priv}}.
key_pairs(S) -> maps:get(key_pairs, S, #{}).

set_account(Account, Trees) ->
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    aec_trees:set_accounts(Trees, AccTree).

trees(#{} = S) ->
    maps:get(trees, S, aec_trees:new()).

-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).
default_tx_env(Height) ->
    Env = aetx_env:set_beneficiary(aetx_env:tx_env(Height), ?BENEFICIARY_PUBKEY),
    aetx_env:set_dry_run(Env, true).
