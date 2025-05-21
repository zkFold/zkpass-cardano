module ZkPass.Cardano.Example.ZkPassResult where

import qualified Data.ByteString.Char8 as BS8
import           PlutusLedgerApi.V3    (BuiltinByteString, toBuiltin)
import           Prelude               (IO, Integer, return, show, (.), (>>=))
import           System.Random         (randomRIO)
--import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

-- | Mock input from ZKPass result.
zkPassResult :: IO BuiltinByteString
zkPassResult = randomRIO (1 :: Integer, 10000) >>= return . toBuiltin . BS8.pack . show
