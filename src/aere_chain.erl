-module(aere_chain).

-include("../node/apps/aecore/include/blocks.hrl").

-export([ new_account/2
        , new_account/3
        , init_accounts/2
        , get_balance/2
        , ensure_contract_code/5
        , default_tx_env/1
        ]).

-type account_init_spec() ::
        #{ pubkey => aec_accounts:pubkey()
         , balance => non_neg_integer()
         }.

-export_type([account_init_spec/0]).

-define(BENEFICIARY_PUBKEY, <<12345:?BENEFICIARY_PUB_BYTES/unit:8>>).


-spec new_account(Balance, Trees) -> {aec_keys:pubkey(), Trees} when
      Balance :: non_neg_integer(),
      Trees :: aec_trees:trees().
new_account(Balance, Trees) ->
    #{public := PubKey} = enacl:sign_keypair(),
    {PubKey, new_account(PubKey, Balance, Trees)}.


-spec new_account(PubKey, Balance, Trees) -> Trees when
      PubKey :: aec_accounts:pubkey(),
      Balance :: non_neg_integer(),
      Trees :: aec_trees:trees().
new_account(PubKey, Balance, Trees) ->
    Trees1 = set_account(aec_accounts:new(PubKey, Balance), Trees),
    Trees1.


-spec init_accounts(Accounts, Trees) -> Trees when
      Accounts :: [account_init_spec()],
      Trees :: aec_trees:trees().
init_accounts([], Trees) ->
    Trees;
init_accounts([#{pubkey := PK, balance := Balance}|Rest], Trees0) ->
    Account = aec_accounts:new(PK, Balance),
    Trees1 = set_account(Account, Trees0),
    init_accounts(Rest, Trees1).


-spec get_balance(PubKey, Trees) -> non_neg_integer() when
      PubKey :: aec_accounts:pubkey(),
      Trees  :: aec_trees:trees().

get_balance(PubKey, Trees) ->
    AccTree = aec_trees:accounts(Trees),
    case aec_accounts_trees:lookup(PubKey, AccTree) of
        none -> 0;
        {value, Account} ->
            aec_accounts:balance(Account)
    end.


-spec set_account(aec_accounts:account(), Trees) -> Trees when
      Trees :: aec_trees:trees().
set_account(Account, Trees) ->
    AccTree = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    aec_trees:set_accounts(Trees, AccTree).


%% Ensures that there exists a contract with given creator, creation nonce and code.  If there
%% isn't, creates a new one. If there is, replaces its code with the provided.
-spec ensure_contract_code(Creator, Nonce, Version, Code, Trees) -> Trees when
      Creator  :: aec_keys:pubkey(),
      Nonce    :: aect_contracts:ct_nonce(),
      Version  :: aect_contracts:vm_version(),
      Code     :: map(),
      Trees    :: aec_trees:trees().

ensure_contract_code(Creator, Nonce, Version, Code, Trees) ->
    PubKey = aect_contracts:compute_contract_pubkey(Creator, Nonce),
    SerCode = aeser_contract_code:serialize(Code),
    CTree0 = aec_trees:contracts(Trees),
    Contract1 =
        case aect_state_tree:lookup_contract(PubKey, CTree0, [no_store]) of
            {value, Contract} ->
                aect_contracts:set_code(SerCode, Contract);
            none ->
                aect_contracts:new(Creator, Nonce, Version, SerCode, 0)
        end,
    CTree1 = aect_state_tree:enter_contract(Contract1, CTree0),
    Trees1 = aec_trees:set_contracts(Trees, CTree1),
    {Contract1, Trees1}.


-spec default_tx_env(aec_blocks:height()) -> aetx_env:env().
default_tx_env(Height) ->
    Env = aetx_env:set_beneficiary(aetx_env:tx_env(Height), ?BENEFICIARY_PUBKEY),
    aetx_env:set_dry_run(Env, true).
