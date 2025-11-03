# 🎲 NFT Raffle Smart Contract

A decentralized raffle system built on Stacks blockchain where users can enter raffles to win unique NFTs! 🏆

## 🌟 Features

- 🎫 **Create Raffles**: Anyone can create a raffle with custom entry fees and participant limits
- 🎯 **Enter Raffles**: Users pay STX to enter active raffles
- 🎰 **Fair Random Selection**: Provably fair winner selection using blockchain randomness
- 🏅 **NFT Prizes**: Winners automatically receive unique NFTs
- 📊 **Transparent Stats**: View raffle statistics and participation status
- 🔄 **NFT Transfers**: Winners can transfer their NFTs to others

## 🚀 Quick Start

### Creating a Raffle

```clarity
(contract-call? .nfts-collectibles create-raffle u1000000 u10 "Golden Dragon NFT")
```

- Entry fee: 1 STX (1,000,000 microSTX)
- Max participants: 10
- Prize name: "Golden Dragon NFT"

### Entering a Raffle

```clarity
(contract-call? .nfts-collectibles enter-raffle u1)
```

### Drawing a Winner

```clarity
(contract-call? .nfts-collectibles draw-winner u1)
```

Only the raffle creator can draw the winner!

## 📋 Contract Functions

### 🔧 Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-raffle` | Create a new raffle | `entry-fee`, `max-participants`, `prize-name` |
| `enter-raffle` | Enter an active raffle | `raffle-id` |
| `draw-winner` | Select winner and mint NFT | `raffle-id` |
| `cancel-raffle` | Cancel raffle (creator only) | `raffle-id` |
| `transfer-nft` | Transfer NFT to another user | `nft-id`, `recipient` |

### 👀 Read-Only Functions

| Function | Description | Returns |
|----------|-------------|---------|
| `get-raffle` | Get raffle details | Raffle data |
| `get-raffle-stats` | Get raffle statistics | Stats object |
| `has-user-entered` | Check if user entered raffle | Boolean |
| `get-nft-owner` | Get NFT owner | Principal |
| `get-active-raffles` | List active raffles | List of raffle IDs |

## 🎮 Usage Examples

### 1. Create Your First Raffle

```clarity
;; Create a raffle with 0.5 STX entry fee, max 5 participants
(contract-call? .nfts-collectibles create-raffle u500000 u5 "Rare Collectible")
```

### 2. Check Raffle Status

```clarity
;; Get raffle information
(contract-call? .nfts-collectibles get-raffle-stats u1)
```

### 3.

