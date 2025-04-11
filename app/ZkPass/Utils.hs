module ZkPass.Utils where

import           Cardano.Api          (parseAddressAny)
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as BL
import           GeniusYield.Types
import           Prelude
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.IO            (IOMode (AppendMode), withFile)
import           Text.Parsec          (parse)

import           ZkPass.Api.Context   (SetupParams)


-- | GYToken as a tuple of strings.
asTuple :: GYAssetClass -> (String, String)
asTuple GYLovelace    = ("", "Lovelace")
asTuple (GYToken p t) = (trim $ show p, trim . show $ tokenNameToHex t)
  where
    trim = reverse . drop 1 . reverse . drop 1

-- | Derive GYAddress
fromAddrHex :: String -> GYAddress
fromAddrHex addrHex = case parse parseAddressAny "" addrHex of
  Right addr -> addressFromApi addr
  Left err   -> error $ show err

-- | Log setup parameters; useful for debugging.
logSetupParams :: SetupParams -> IO ()
logSetupParams sp = do
  createDirectoryIfMissing True "./log"
  withFile ("log" </> "plonkup-raw-contract-data.json") AppendMode $ \h -> BL.hPut h (encode sp)
