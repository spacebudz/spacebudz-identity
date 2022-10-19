import {
  Address,
  Constr,
  Data,
  Lucid,
  PolicyId,
  TxHash,
  utf8ToHex,
} from "lucid-cardano";
import scripts from "../onchain/scripts.json";

const {
  gateValidator,
  controlPolicy,
  identityPolicy,
  controlValidator,
} = scripts;

const contractDetails = {
  ownershipPrefix: "SpaceBud",
  ownershipCs: "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc",
  oref: {
    txHash: "b26a00bc3e795092f08ffeda4e8d74ae64a9f0690c7edbdc049f7beed4f54046",
    outputIndex: 2,
  },
  identityPrefix: "Identity",
};

export class Contract {
  private lucid!: Lucid;

  constructor(lucid: Lucid) {
    this.lucid = lucid;
  }

  private gateAddress(): Address {
    return this.lucid.utils.validatorToAddress({
      type: "PlutusV1",
      script: gateValidator,
    });
  }

  private controlAddress(): Address {
    return this.lucid.utils.validatorToAddress({
      type: "PlutusV1",
      script: controlValidator,
    });
  }

  private controlPolicyId(): PolicyId {
    return this.lucid.utils.validatorToScriptHash({
      type: "PlutusV1",
      script: controlPolicy,
    });
  }

  private identityPolicyId(): PolicyId {
    return this.lucid.utils.validatorToScriptHash({
      type: "PlutusV1",
      script: identityPolicy,
    });
  }

  async deploy(): Promise<TxHash> {
    const utxos = await this.lucid.wallet.getUtxos();
    const utxo = utxos.find(
      (utxo) =>
        utxo.txHash === contractDetails.oref.txHash &&
        utxo.outputIndex === contractDetails.oref.outputIndex,
    );
    if (!utxo) throw new Error("Utxo is required to deploy contract");

    const tx = this.lucid.newTx();

    for (let i = 0; i < 10000; i += 100) {
      const unit = this.controlPolicyId() + utf8ToHex(i.toString());
      tx.mintAssets({ [unit]: 1n }, Data.empty());
      tx.payToContract(this.controlAddress(), Data.to(new Map()), {
        [unit]: 1n,
      });
    }

    const txComplete = await tx
      .collectFrom([utxo])
      .attachMintingPolicy({ type: "PlutusV1", script: controlPolicy })
      .complete();

    const signedTx = await txComplete.sign().complete();

    const txHash = await signedTx.submit();
    return txHash;
  }

  async updateIdentity(identityId: number, metadata: any): Promise<TxHash> {
    const ownershipUnit = contractDetails.ownershipCs +
      utf8ToHex(contractDetails.ownershipPrefix + identityId);

    const controlId = Math.floor(identityId / 100) * 100;
    const controlUnit = this.controlPolicyId() +
      utf8ToHex(controlId.toString());

    const identityUnit = this.identityPolicyId() +
      utf8ToHex(contractDetails.identityPrefix + identityId);

    const [controlUtxo] = await this.lucid.utxosAtWithUnit(
      this.controlAddress(),
      controlUnit,
    );

    const ownershipUtxo = (await this.lucid.wallet.getUtxos()).find((utxo) =>
      Object.keys(utxo.assets).includes(ownershipUnit)
    );

    if (!ownershipUtxo) {
      throw new Error(
        "You are not the holder of " +
          contractDetails.ownershipPrefix +
          " #" +
          identityId,
      );
    }

    const controlMap = Data.from(
      await this.lucid.datumOf(controlUtxo),
    ) as Map<bigint, bigint>;

    if (controlMap.has(BigInt(identityId))) {
      /** Identity token already exists; just move token inside the gate address */
      const [gateUtxo] = await this.lucid.utxosAtWithUnit(
        this.gateAddress(),
        identityUnit,
      );
      const tx = await this.lucid
        .newTx()
        .collectFrom([ownershipUtxo])
        .collectFrom([gateUtxo], Data.empty())
        .payToContract(this.gateAddress(), Data.empty(), { [identityUnit]: 1n })
        .attachMetadataWithConversion(537, {
          ["0x" + this.identityPolicyId()]: {
            ["0x" + utf8ToHex(contractDetails.identityPrefix + identityId)]: {
              ...metadata,
            },
          },
        })
        .attachSpendingValidator({ type: "PlutusV1", script: gateValidator })
        .complete();
      const signedTx = await tx.sign().complete();
      return await signedTx.submit();
    } else {
      /** Mint the identity token first */
      const updatedMap = controlMap.set(
        BigInt(identityId),
        1n,
      );
      const tx = await this.lucid
        .newTx()
        .collectFrom([ownershipUtxo])
        .collectFrom(
          [controlUtxo],
          Data.to(new Constr(0, [BigInt(controlId), BigInt(identityId)])),
        )
        .mintAssets({ [identityUnit]: 1n }, Data.empty())
        .payToContract(
          this.controlAddress(),
          Data.to(updatedMap),
          controlUtxo.assets,
        )
        .payToContract(this.gateAddress(), Data.empty(), { [identityUnit]: 1n })
        .attachMetadataWithConversion(537, {
          ["0x" + this.identityPolicyId()]: {
            ["0x" + utf8ToHex(contractDetails.identityPrefix + identityId)]: {
              ...metadata,
            },
          },
        })
        .attachSpendingValidator({ type: "PlutusV1", script: controlValidator })
        .attachMintingPolicy({ type: "PlutusV1", script: identityPolicy })
        .complete();
      const signedTx = await tx.sign().complete();
      return await signedTx.submit();
    }
  }

  async getIdentity(identityId: number): Promise<any | undefined> {
    const identityUnit = this.identityPolicyId() +
      utf8ToHex(contractDetails.identityPrefix + identityId);
    const txHash = await fetch(
      `https://cardano-${
        this.lucid.network === "Mainnet" ? "mainnet" : "testnet"
      }.blockfrost.io/api/v0/assets/${identityUnit}/transactions?order=desc&count=1`,
      { headers: { project_id: (this.lucid.provider as any).projectId } },
    )
      .then((res) => res.json())
      .then((res) => res[0]?.tx_hash);
    if (!txHash || txHash.error) return undefined;

    const txMetadata = await fetch(
      `https://cardano-${
        this.lucid.network === "Mainnet" ? "mainnet" : "testnet"
      }.blockfrost.io/api/v0/txs/${txHash}/metadata`,
      { headers: { project_id: (this.lucid.provider as any).projectId } },
    ).then((res) => res.json());

    const metadata = (txMetadata || []).find((m: any) => m.label == 537)
      ?.json_metadata;

    if (!metadata) return undefined;

    return (
      metadata?.[this.identityPolicyId()]?.[
        utf8ToHex(contractDetails.identityPrefix + identityId)
      ] ||
      metadata?.["0x" + this.identityPolicyId()]?.[
        "0x" + utf8ToHex(contractDetails.identityPrefix + identityId)
      ]
    );
  }
}

export * from "lucid-cardano";
