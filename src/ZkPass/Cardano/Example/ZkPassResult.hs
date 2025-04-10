module ZkPass.Cardano.Example.ZkPassResult where

import           PlutusLedgerApi.V3           (BuiltinByteString)
import           Prelude                      (IO, Integer, return, (.), (>>=))
import           System.Random                (randomRIO)
import           ZkFold.Cardano.OnChain.Utils (dataToBlake)

-- | Mock input from ZKPass result.
zkPassResult :: IO BuiltinByteString
zkPassResult = randomRIO (1 :: Integer, 10000) >>= return . dataToBlake
