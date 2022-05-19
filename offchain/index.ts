import {
  Construct,
  Data,
  Lucid,
  Tx,
  TxHash,
  validatorToAddress,
  validatorToScriptHash,
} from 'lucid-cardano';
import scripts from '../onchain/scripts.json';

const assetName = (utf8: string | number) =>
  Buffer.from(utf8.toString()).toString('hex');

const {
  gateValidator,
  controlPolicy,
  identityPolicy,
  controlValidator,
} = scripts;

const contractDetails = {
  ownershipPrefix: 'SpaceBud',
  ownershipCs: 'd5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc',
  oref: {
    txHash: 'b26a00bc3e795092f08ffeda4e8d74ae64a9f0690c7edbdc049f7beed4f54046',
    outputIndex: 2,
  },
  identityPrefix: 'Identity',
};

const gateAddress = () =>
  validatorToAddress({
    type: 'PlutusV1',
    script: gateValidator,
  });

const controlAddress = () =>
  validatorToAddress({
    type: 'PlutusV1',
    script: controlValidator,
  });

const controlPolicyId = () =>
  validatorToScriptHash({
    type: 'PlutusV1',
    script: controlPolicy,
  });

const identityPolicyId = () =>
  validatorToScriptHash({
    type: 'PlutusV1',
    script: identityPolicy,
  });

export const deploy = async (): Promise<TxHash> => {
  const utxos = await Lucid.wallet.getUtxos();
  const utxo = utxos.find(
    utxo =>
      utxo.txHash === contractDetails.oref.txHash &&
      utxo.outputIndex === contractDetails.oref.outputIndex
  );
  if (!utxo) throw new Error('Utxo is required to deploy contract');

  const tx = await Tx.new();

  for (let i = 0; i < 10000; i += 100) {
    const unit = controlPolicyId + assetName(i);
    tx.mintAssets({ [unit]: 1n }, Data.empty());
    tx.payToContract(controlAddress(), Data.to(new Map()), {
      [unit]: 1n,
    });
  }

  const txComplete = await tx
    .collectFrom([utxo])
    .attachMintingPolicy({ type: 'PlutusV1', script: controlPolicy })
    .complete();

  const signedTx = await txComplete.sign().complete();

  const txHash = await signedTx.submit();
  return txHash;
};

export const updateIdentity = async (
  identityId: number,
  metadata: any
): Promise<TxHash> => {
  const ownershipUnit =
    contractDetails.ownershipCs +
    assetName(contractDetails.ownershipPrefix + identityId);

  const controlId = Math.floor(identityId / 100) * 100;
  const controlUnit = controlPolicyId + assetName(controlId);

  const identityUnit =
    identityPolicyId + assetName(contractDetails.identityPrefix + identityId);

  const [controlUtxo] = await Lucid.utxosAtWithUnit(
    controlAddress(),
    controlUnit
  );

  const ownershipUtxo = (await Lucid.wallet.getUtxos()).find(utxo =>
    Object.keys(utxo.assets).includes(ownershipUnit)
  );

  if (!ownershipUtxo)
    throw new Error(
      'You are not the holder of ' +
        contractDetails.ownershipPrefix +
        ' #' +
        identityId
    );

  const controlMap: Map<BigInt, BigInt> = Data.from(
    await Lucid.datumOf(controlUtxo)
  );

  if (controlMap.has(BigInt(identityId))) {
    /** Identity token already exists; just move token inside the gate address */
    const [gateUtxo] = await Lucid.utxosAtWithUnit(gateAddress(), identityUnit);
    const tx = await Tx.new()
      .collectFrom([ownershipUtxo])
      .collectFrom([gateUtxo], Data.empty())
      .payToContract(gateAddress(), Data.empty(), { [identityUnit]: 1n })
      .attachMetadata(537, {
        [identityPolicyId()]: {
          [assetName(contractDetails.identityPrefix + identityId)]: {
            ...metadata,
          },
        },
      })
      .attachSpendingValidator({ type: 'PlutusV1', script: gateValidator })
      .complete();
    const signedTx = await tx.sign().complete();
    return await signedTx.submit();
  } else {
    /** Mint the identity token first */
    const updatedMap = controlMap.set(BigInt(identityId), 1n);
    const tx = await Tx.new()
      .collectFrom([ownershipUtxo])
      .collectFrom(
        [controlUtxo],
        Data.to(new Construct(0, [controlId, identityId]))
      )
      .mintAssets({ [identityUnit]: 1n }, Data.empty())
      .payToContract(controlAddress(), Data.to(updatedMap), controlUtxo.assets)
      .payToContract(gateAddress(), Data.empty(), { [identityUnit]: 1n })
      .attachMetadata(537, {
        [identityPolicyId()]: {
          [assetName(contractDetails.identityPrefix + identityId)]: {
            ...metadata,
          },
        },
      })
      .attachSpendingValidator({ type: 'PlutusV1', script: controlValidator })
      .attachMintingPolicy({ type: 'PlutusV1', script: identityPolicy })
      .complete();
    const signedTx = await tx.sign().complete();
    return await signedTx.submit();
  }
};

export const getIdentity = async (
  identityId: number
): Promise<any | undefined> => {
  const identityUnit =
    identityPolicyId + assetName(contractDetails.identityPrefix + identityId);
  const txHash = await fetch(
    `https://cardano-${
      Lucid.network === 'Mainnet' ? 'mainnet' : 'testnet'
    }.blockfrost.io/api/v0/assets/${identityUnit}/transactions?order=desc&count=1`,
    { headers: { project_id: Lucid.provider.projectId } }
  )
    .then(res => res.json())
    .then(res => res[0]?.tx_hash);
  if (!txHash || txHash.error) return undefined;

  const txMetadata = await fetch(
    `https://cardano-${
      Lucid.network === 'Mainnet' ? 'mainnet' : 'testnet'
    }.blockfrost.io/api/v0/txs/${txHash}/metadata`,
    { headers: { project_id: Lucid.provider.projectId } }
  ).then(res => res.json());

  const metadata = (txMetadata || []).find((m: any) => m.label == 537)
    ?.json_metadata;

  if (!metadata) return undefined;

  return metadata?.[identityPolicyId()]?.[
    assetName(contractDetails.identityPrefix + identityId)
  ];
};

export * from 'lucid-cardano';
