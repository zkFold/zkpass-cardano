module Main where

import           Cardano.Api                            (AssetName (..),
                                                         UsingRawBytesHex (..))
import           Data.Aeson                             (decode)
import           Data.ByteString                        as BS (writeFile)
import qualified Data.ByteString.Lazy                   as BL
import           Data.Maybe                             (fromJust)
import           Data.String                            (IsString (..))
import           PlutusLedgerApi.V3                     (fromBuiltin)
import           Prelude                                (IO, Integer, putStr,
                                                         return, show, ($),
                                                         (++), (.), (<$>),
                                                         (>>=))
import           System.FilePath                        ((</>))
import           System.Random                          (randomRIO)
import           ZkFold.Cardano.OffChain.Utils          (dataToCBOR)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F     as F
import           ZkFold.Cardano.OnChain.Plonkup.Data    (InputBytes)
import           ZkFold.Cardano.OnChain.Utils           (dataToBlake)
import           ZkPass.Cardano.Example.IdentityCircuit (IdentityCircuitContract (..),
                                                         zkPassResultVerificationBytes)


-- | Sample input from ZKPass result.
zkPassResultInput :: IO InputBytes
zkPassResultInput = randomRIO (1 :: Integer, 10000) >>= return . F.toInput . dataToBlake

main :: IO ()
main = do
  let path = "."

  zkpr <- zkPassResultInput

  IdentityCircuitContract{..} <- fromJust . decode <$> BL.readFile (path </> "test-data" </> "plonkup-raw-contract-data.json")

  putStr $ "x: " ++ show x ++ "\n" ++ "ps: " ++ show ps ++ "\n"

  let (_, input, proof) = zkPassResultVerificationBytes x ps zkpr

  BS.writeFile (path </> "assets" </> "tokenname") $ fromString $ show $ UsingRawBytesHex $ AssetName $ fromBuiltin $ F.fromInput input
  BS.writeFile (path </> "assets" </> "unit.cbor") $ dataToCBOR ()
  BS.writeFile (path </> "assets" </> "redeemerZkPassToken.cbor") $ dataToCBOR proof
