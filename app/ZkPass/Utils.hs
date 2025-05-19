module ZkPass.Utils where

import           Cardano.Api       (parseAddressAny)
import           GeniusYield.Types
import           Prelude
import           Text.Parsec       (parse)


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
