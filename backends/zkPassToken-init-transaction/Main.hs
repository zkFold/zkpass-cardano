module Main where

import           Data.Aeson                             (encode)
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BL
import           Prelude                                (Bool (..), IO,
                                                         Show (..), putStr, ($),
                                                         (++), (.))
import           System.Directory                       (createDirectoryIfMissing)
import           System.FilePath                        ((</>))
import qualified System.IO                              as IO
import           System.Random                          (randomRIO)
import           Test.QuickCheck.Arbitrary              (Arbitrary (..))
import           Test.QuickCheck.Gen                    (generate)

import           ZkFold.Cardano.OffChain.Utils          (currencySymbolOf,
                                                         dataToCBOR, savePlutus)
import           ZkPass.Cardano.Example.IdentityCircuit (IdentityCircuitContract (..),
                                                         identityCircuitVerificationBytes)
import           ZkPass.Cardano.UPLC.ZkPassToken        (forwardingMintCompiled,
                                                         zkPassTokenCompiled)

main :: IO ()
main = do
  let path = "."

  x'  <- generate arbitrary
  ps' <- generate arbitrary

  let contract = IdentityCircuitContract x' ps'

  createDirectoryIfMissing True $ path </> "test-data"
  createDirectoryIfMissing True $ path </> "assets"

  BL.writeFile (path </> "test-data" </> "plonkup-raw-contract-data.json") $ encode contract

  putStr $ "x: " ++ show x' ++ "\n" ++ "ps: " ++ show ps' ++ "\n"

  let (setup, _, _) = identityCircuitVerificationBytes x' ps'

  fmTag <- randomRIO (1, 10000)

  savePlutus (path </> "assets" </> "zkPassToken.plutus") $ zkPassTokenCompiled setup
  savePlutus (path </> "assets" </> "forwardingMint.plutus") $ forwardingMintCompiled fmTag

  BS.writeFile (path </> "assets" </> "datumZkPassToken.cbor") . dataToCBOR . currencySymbolOf $ zkPassTokenCompiled setup

  IO.writeFile (path </> "assets" </> "parkingTag.txt") $ show fmTag
