module Contract (controlValidatorSerialized, controlPolicySerialized, identityPolicySerialized, gateValidatorSerialized) where

import Prelude (String)
import Cardano.Api hiding (Value, TxOut, Address)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInfo (..), TxInInfo(..), TxOut (..), TxOutRef (..), ownCurrencySymbol, getContinuingOutputs, findDatum, findOwnInput)
import Plutus.V1.Ledger.Value (CurrencySymbol (..), TokenName (..), flattenValue, singleton, Value, valueOf, symbols)
import Plutus.V1.Ledger.Scripts (mkMintingPolicyScript)
import Plutus.V1.Ledger.Tx (isPayToScriptOut)
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Value (noAdaValue)
import Ledger.Address (scriptAddress)
import Plutus.V1.Ledger.Api (unValidatorScript,unMintingPolicyScript, Credential (..), Address (..), Datum (..), PubKeyHash (..), DatumHash)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as M


contractDetails :: ContractDetails
contractDetails = ContractDetails { ownershipPrefix = "SpaceBud"
                                  , ownershipCs = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc"
                                  , oref = TxOutRef "b26a00bc3e795092f08ffeda4e8d74ae64a9f0690c7edbdc049f7beed4f54046" 2
                                  , identityPrefix = "Identity"
                                  }


mkControlPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkControlPolicy oref _ ctx = hasUtxo
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    hasUtxo :: Bool
    hasUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs txInfo


mkIdentityPolicy :: CurrencySymbol -> () -> ScriptContext -> Bool
mkIdentityPolicy controlCs () ctx = checkControlToken
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    checkControlToken :: Bool
    checkControlToken = controlCs `elem` symbols (valueProducedAtScripts txInfo)


mkControlValidator :: (CurrencySymbol, (CurrencySymbol, BuiltinByteString), (CurrencySymbol, BuiltinByteString), Address) -> ControlMap -> (Integer, Integer) -> ScriptContext -> Bool
mkControlValidator (controlCs, (identityCs, identityPrefix), (ownershipCs, ownershipPrefix), gateAddress) controlMap (controlId, identityId) ctx = checkRangeAndUniqueness && 
                                                                                                                    checkMint && 
                                                                                                                    checkControlToken && 
                                                                                                                    checkOwnershipToken &&
                                                                                                                    checkOutput
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    identityIdBytes :: BuiltinByteString
    identityIdBytes = integerToByteString identityId

    identityToken :: Value
    identityToken = singleton identityCs (TokenName (appendByteString identityPrefix identityIdBytes)) 1

    controlIdBytes :: BuiltinByteString
    controlIdBytes = integerToByteString controlId

    controlToken :: Value
    controlToken = singleton controlCs (TokenName controlIdBytes) 1

    getDatum :: PlutusTx.FromData a => DatumHash -> a
    getDatum h = case findDatum h txInfo of
          Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
            Just b -> b

    ownOutputValue :: Value
    ownOutputDatum :: ControlMap
    (ownOutputValue, ownOutputDatum) = case getContinuingOutputs ctx of
        [o] -> (txOutValue o, getDatum (fromJust (txOutDatumHash o)))

    gateOutputValue :: Value
    gateOutputDatum :: ()
    (gateOutputValue, gateOutputDatum) = let [(h,v)] = scriptOutputsAt gateAddress txInfo in (v, getDatum h)

    checkRangeAndUniqueness :: Bool
    checkRangeAndUniqueness = controlId <= identityId && identityId < controlId + 100 && not (M.member identityId controlMap)

    checkMint :: Bool
    checkMint = txInfoMint txInfo == identityToken

    checkControlToken :: Bool
    checkControlToken = let Just input = findOwnInput ctx in 
                valueOf (txOutValue (txInInfoResolved input)) controlCs (TokenName controlIdBytes) == 1

    checkOwnershipToken :: Bool
    checkOwnershipToken = any (== True) [valueOf (txOutValue (txInInfoResolved i)) ownershipCs (TokenName (ownershipPrefix <> identityIdBytes)) >= 1 | i <- txInfoInputs txInfo]

    checkOutput :: Bool
    checkOutput = noAdaValue ownOutputValue == controlToken && M.insert identityId 1 controlMap == ownOutputDatum &&
                 noAdaValue gateOutputValue == identityToken && gateOutputDatum == ()
                


mkGateValidator :: (CurrencySymbol, BuiltinByteString, BuiltinByteString) -> () -> () -> ScriptContext -> Bool
mkGateValidator (ownershipCs, ownershipPrefix, identityPrefix) () () ctx = checkOwnershipToken && checkOutput
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    identityIdBytes :: BuiltinByteString
    identityIdBytes = let [(_,TokenName tn,_)] = flattenValue (noAdaValue ownInputValue)
                          lengthPrefix = lengthOfByteString identityPrefix
                          sliceLength = lengthOfByteString tn - lengthPrefix
                       in sliceByteString lengthPrefix sliceLength tn

    getDatum :: PlutusTx.FromData a => DatumHash -> a
    getDatum h = case findDatum h txInfo of
          Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
            Just b -> b

    ownOutputValue :: Value
    ownOutputDatum :: ()
    (ownOutputValue, ownOutputDatum) = case getContinuingOutputs ctx of
        [o] -> (txOutValue o, getDatum (fromJust (txOutDatumHash o)))

    ownInputValue :: Value
    ownInputValue = let Just input = findOwnInput ctx in (txOutValue (txInInfoResolved input))

    checkOwnershipToken :: Bool
    checkOwnershipToken = any (== True) [valueOf (txOutValue (txInInfoResolved i)) ownershipCs (TokenName (ownershipPrefix <> identityIdBytes)) >= 1 | i <- txInfoInputs txInfo]

    checkOutput :: Bool
    checkOutput = ownInputValue == ownOutputValue && ownOutputDatum == ()

-- Instances

controlPolicy :: Scripts.MintingPolicy
controlPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode (oref contractDetails)
    where 
        wrap oref = Scripts.wrapMintingPolicy $ mkControlPolicy oref

controlPolicyId :: CurrencySymbol
controlPolicyId = scriptCurrencySymbol $ controlPolicy

identityPolicy :: Scripts.MintingPolicy
identityPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode (controlPolicyId)
    where 
        wrap cs = Scripts.wrapMintingPolicy $ mkIdentityPolicy cs

identityPolicyId :: CurrencySymbol
identityPolicyId = scriptCurrencySymbol $ identityPolicy

data GateValidator
instance Scripts.ValidatorTypes GateValidator where
    type instance RedeemerType GateValidator = ()
    type instance DatumType GateValidator = ()

gateInstance :: Scripts.TypedValidator GateValidator 
gateInstance = Scripts.mkTypedValidator @GateValidator
    ($$(PlutusTx.compile [|| mkGateValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode (ownershipCs contractDetails, ownershipPrefix contractDetails, identityPrefix contractDetails))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

gateValidator :: Scripts.Validator
gateValidator = Scripts.validatorScript gateInstance

gateAddress :: Address
gateAddress = scriptAddress gateValidator


data ControlValidator
instance Scripts.ValidatorTypes ControlValidator where
    type instance DatumType ControlValidator = ControlMap
    type instance RedeemerType ControlValidator = (Integer, Integer)

controlValidatorInstance :: Scripts.TypedValidator ControlValidator
controlValidatorInstance = Scripts.mkTypedValidator @ControlValidator
    ($$(PlutusTx.compile [|| mkControlValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode (controlPolicyId, (identityPolicyId, identityPrefix contractDetails), (ownershipCs contractDetails, ownershipPrefix contractDetails), gateAddress))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ControlMap @(Integer, Integer)

controlValidator :: Scripts.Validator
controlValidator = Scripts.validatorScript controlValidatorInstance


-- Utils

integerToByteString :: Integer -> BuiltinByteString
integerToByteString n
  | n == 0 = "0"
  | n == 1 = "1"
  | n == 2 = "2"
  | n == 3 = "3"
  | n == 4 = "4"
  | n == 5 = "5"
  | n == 6 = "6"
  | n == 7 = "7"
  | n == 8 = "8"
  | n == 9 = "9"
  | otherwise = integerToByteString (n `divide` 10) <> integerToByteString (n `modulo` 10)


scriptOutputsAt :: Address -> TxInfo -> [(DatumHash, Value)]
scriptOutputsAt address p =
    let flt TxOut{txOutDatumHash=Just ds, txOutAddress=address', txOutValue} | address == address' = Just (ds, txOutValue)
        flt _ = Nothing
    in mapMaybe flt (txInfoOutputs p)

valueProducedAtScripts :: TxInfo -> Value
valueProducedAtScripts ptx = foldMap txOutValue (filter isPayToScriptOut (txInfoOutputs ptx))

fromJust :: Maybe a -> a
fromJust Nothing  = error ()
fromJust (Just x) = x

-- Data

type ControlMap = M.Map Integer Integer

data ContractDetails = ContractDetails {
    ownershipPrefix :: !BuiltinByteString,
    ownershipCs :: !CurrencySymbol,
    oref :: !TxOutRef,
    identityPrefix :: !BuiltinByteString
}

-- Serialization

controlValidatorSerialized :: String
controlValidatorSerialized = C.unpack $ B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ unValidatorScript controlValidator) :: PlutusScript PlutusScriptV1)

controlPolicySerialized :: String
controlPolicySerialized = C.unpack $ B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ unMintingPolicyScript controlPolicy) :: PlutusScript PlutusScriptV1)

identityPolicySerialized :: String
identityPolicySerialized = C.unpack $ B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ unMintingPolicyScript identityPolicy) :: PlutusScript PlutusScriptV1)

gateValidatorSerialized :: String
gateValidatorSerialized = C.unpack $ B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ unValidatorScript gateValidator) :: PlutusScript PlutusScriptV1)