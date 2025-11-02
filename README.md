StackSafe
A Clarity Smart Contract for Secure STX Storage and Retrieval

Overview
StackSafe is a Clarity-based smart contract built on the Stacks blockchain. It provides a secure and transparent way for users to store, lock, and withdraw STX tokens without relying on centralized systems. The contract ensures that all actions are verifiable on-chain and protected by ownership logic.

Features
Secure Deposits — Users can safely deposit STX directly into the contract.
Withdrawals — Only the owner of the deposited funds can withdraw.
Balance Checking — Anyone can view their on-chain balance stored in StackSafe.
Upgradeable Logic (Future) — Support for features like time-locks, multi-user access, and event logging.

How It Works

Deposit: A user calls the deposit-stx function to send tokens into the vault.

Store Mapping: The contract records the deposit under the sender’s principal.

Withdraw: The same user can later call withdraw-stx to retrieve their tokens.

Read Functions: get-balance and get-total-balance allow public read-only access to stored balances.
