# zkFold x zkPass: Bring zkPass to Cardano

**zkPass** on Cardano is a smart contract that enables developers to post, verify, and utilize zkPass oracle attestations on the Cardano blockchain. It is a tool that makes new types of DApps possible on Cardano.

This repository contains the zkPass onchain code, as well as the server side of the zkPass prototype DApp.

## zkPass server

To run the zkPass server, execute:
```shell
cabal run zkpass-server -- config.json
```
where `config.json` contains your configuration for network and provider.  (File `config-template.json` provides a template configuration.)

The zkPass server on this repository was written using the [Atlas](https://atlas-app.io) framework.

## zkPass client

The zkPass client can be found [here](https://github.com/zkFold/zkpass-client).