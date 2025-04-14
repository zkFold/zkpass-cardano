# zkFold x zkPass: Bring zkPass to Cardano

**zkPass** on Cardano is a smart contract that enables developers to post, verify, and utilize zkPass oracle attestations on the Cardano blockchain. It is a tool that makes new types of DApps possible on Cardano.

This repository contains the zkPass onchain code, as well as the server side of the zkPass prototype DApp.

## Requirements

Compilation was tested with GHC 9.6.6 and Cabal 3.10.2.1.  Other library requirements are described in [this](https://github.com/input-output-hk/cardano-node-wiki/blob/602fe3a56a13a773cd6c0e00420ee3e5c56f2857/docs/getting-started/install.md) guide.  Additionally, `libpq-dev` or `postgresql` need to be installed as otherwise an error suggesting missing pg_config can occur.

## zkPass server

To run the zkPass server, execute:
```shell
cabal run zkpass-server -- config.json
```
where `config.json` contains your configuration for network and provider.  (File `config-template.json` provides a template configuration.)

The zkPass server on this repository was written using the [Atlas](https://atlas-app.io) framework.

## zkPass client

The zkPass client can be found [here](https://github.com/zkFold/zkpass-client).