<p align="center">
  <img width="100px" src="https://github.com/SpaceBudz/spacebudz/raw/main/src/images/brand/logo.png" align="center" />
  <h1 align="center">SpaceBudz Identity</h1>
  <p align="center">Personalize your SpaceBudz and give it an identity.</p>

  <p align="center">
    <img src="https://img.shields.io/github/commit-activity/m/SpaceBudz/spacebudz-identity?style=for-the-badge" />
    <a href="https://www.npmjs.com/package/@spacebudz/spacebudz-identity">
      <img src="https://img.shields.io/npm/v/@spacebudz/spacebudz-identity?style=for-the-badge" />
    </a>
    <a href="https://www.npmjs.com/package/@spacebudz/spacebudz-identity">
      <img src="https://img.shields.io/npm/dw/@spacebudz/spacebudz-identity?style=for-the-badge" />
    </a>
    <img src="https://img.shields.io/npm/l/@spacebudz/spacebudz-identity?style=for-the-badge" />
    <a href="https://twitter.com/spacebudzNFT">
      <img src="https://img.shields.io/twitter/follow/spacebudzNFT?style=for-the-badge&logo=twitter" />
    </a>
  </p>

</p>

### Validity

The policy id for the identity tokens:

`e47f849c5262b93b46789251519ec0a7921087b84b3782b871497e8f`


### Usage

```js
import Identity from "@spacebudz/spacebudz-identity"

const lucid = await Identity.Lucid.new(new Identity.Blockfrost("https://cardano-mainnet.blockfrost.io/api/v0", projectId));
const api = await window.cardano.nami.enable();
lucid.selectWallet(api);

const contract = new Identity.Contract(lucid);

// Update the identity for SpaceBud #995
const txHash = await contract.updateIdentity(995, {nickname: "Berry"});

await lucid.awaitTx(txHash);

const identity = await contract.getIdentity(995);
console.log(identity);
```

### NPM

```
npm install @spacebudz/spacebudz-identity
```

### From source

Install dependencies
```
npm install
```
Generate build
```
npm run build
```

### Verify contract

- ghc >= 8.10.7
- cabal >= 3.0
- Install [custom libsodium](https://developers.cardano.org/docs/get-started/installing-cardano-node/#downloading--compiling)

```
npm run build:contract
```
This will generate the serialized scripts under `./src/onchain/scripts.json`

[How the contract works](./docs/README.md)

### Schema

All keys are optional

```json
{
    "nickname": "string",
    "urbit": ["urbit_id"],
    "twitter": ["@profile"],
    "discord": ["username"],
    "email": ["email_address"],
    "color": "string hex color"
}
```