module Main where

import Contract (controlPolicySerialized, controlValidatorSerialized, gateValidatorSerialized, identityPolicySerialized)
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy.IO as I
import GHC.Generics (Generic)
import Prelude

-- Add additional serialized scripts here
data Scripts = Scripts {controlPolicy :: String, controlValidator :: String, gateValidator :: String, identityPolicy :: String}
  deriving (Show, Generic, ToJSON)

scripts :: Scripts
scripts =
  Scripts
    { controlPolicy = controlPolicySerialized,
      controlValidator = controlValidatorSerialized,
      gateValidator = gateValidatorSerialized,
      identityPolicy = identityPolicySerialized
    }

main :: IO ()
main = do
  I.writeFile "scripts.json" (encodeToLazyText scripts)
  putStrLn "Scripts compiled"