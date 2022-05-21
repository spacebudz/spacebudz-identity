<p align="center">
  <img width="100px" src="https://github.com/SpaceBudz/spacebudz/raw/main/src/images/brand/logo.png" align="center" alt="GitHub Readme Stats" />
  <h1 align="center">SpaceBudz Identity</h1>
  <p align="center">Personalize your SpaceBud and give it an identity.</p>
</p>

### Validity

The policy id for identity tokens:

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

### Contract architecture

TODO