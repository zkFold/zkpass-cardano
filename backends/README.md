# Off-chain code for `zkPassToken`

1. `zkPassToken-init-transaction` writes the related Plutus scripts (UPLC code) to files in CBOR format. It also writes token's policy id to a file in CBOR format.
2. `zkPassToken-minting-transaction` constructs the data items used as script parameters and writes them to files in CBOR format.
