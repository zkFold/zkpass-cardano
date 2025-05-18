{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import           GeniusYield.GYConfig      (coreConfigIO, withCfgProviders)
import           Network.Wai.Handler.Warp
import           Prelude
import           System.Environment        (getArgs)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))
import           Test.QuickCheck.Gen       (generate)

import           ZkPass.Api                (app)
import           ZkPass.Api.Context        (Ctx (..), SetupParams (..))
import           ZkPass.Utils              (logSetupParams)


-- | Getting path for our core configuration.
parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    coreCfg: _       -> return coreCfg
    _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

main :: IO ()
main = do
  putStrLn "parsing Config ..."
  coreCfgPath <- parseArgs
  coreCfg     <- coreConfigIO coreCfgPath

  -- x  <- generate arbitrary
  let x  = 35312767206560791369550118312524979198971884430305671696030775049220092579175
  ps <- generate arbitrary
  let setupParams = SetupParams x ps

  -- Uncomment to log setup parameters:
  -- logSetupParams setupParams

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx coreCfg providers
    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx setupParams
