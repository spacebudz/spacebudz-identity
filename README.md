# SpaceBudz Identity

## Get started


### Usage

```js
import {Lucid, Blockfrost, updateIdentity, getIdentity} from "@spacebudz/spacebudz-identity"

await Lucid.initialize(new Blockfrost("https://cardano-mainnet.blockfrost.io/api/v0", projectId));

await Lucid.selectWallet("nami");

// Update the identity for SpaceBud #123
const txHash = updateIdentity(123, {nickname: "Berry"});

await Lucid.awaitTx(txHash);

const identity = await getIdentity(123);
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
This will generate the serialized scripts under `./onchain/scripts.json`


### Schema

All keys are optional

```json
{
    "nickname": "string",
    "urbit": ["urbit_id"],
    "discord": ["username"],
    "email": ["email_address"],
    "color": "string hex color"
}
```