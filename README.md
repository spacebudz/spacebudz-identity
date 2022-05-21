<p align="center">
  <img width="100px" src="https://github.com/SpaceBudz/spacebudz/raw/main/src/images/brand/logo.png" align="center" />
  <h1 align="center">SpaceBudz Identity</h1>
  <p align="center">Personalize your SpaceBudz and give it an identity.</p>

  <p align="center">
    <img src="https://img.shields.io/github/commit-activity/m/SpaceBudz/spacebudz-identity?style=for-the-badge" />
    <a href="https://www.npmjs.com/package/@spacebudz/spacebudz-identity">
      <img src="https://img.shields.io/npm/v/@spacebudz/spacebudz-identity?style=for-the-badge" />
    </a>
    <a href="https://www.npmjs.com/package/lucid-cardano">
      <img src="https://img.shields.io/npm/dw/lucid-cardano?style=for-the-badge" />
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
import {Lucid, Blockfrost, updateIdentity, getIdentity} from "@spacebudz/spacebudz-identity"

await Lucid.initialize(new Blockfrost("https://cardano-mainnet.blockfrost.io/api/v0", projectId));

await Lucid.selectWallet("nami");

// Update the identity for SpaceBud #995
const txHash = updateIdentity(995, {nickname: "Berry"});

await Lucid.awaitTx(txHash);

const identity = await getIdentity(995);
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

```
npm run build:contract
```
This will generate the serialized scripts under `./src/onchain/scripts.json`

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

### [How the contract works](./docs/README.md)