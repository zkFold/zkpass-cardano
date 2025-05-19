module Main where

import           GeniusYield.GYConfig     (coreConfigIO, withCfgProviders)
import           Network.Wai.Handler.Warp
import           Prelude
import           System.Directory         (createDirectoryIfMissing)
import           System.Environment       (getArgs)
import           System.FilePath          ((</>))

import           ZkPass.Api               (app)
import           ZkPass.Api.Context       (Ctx (..))


-- | Getting path for our core configuration.
parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    coreCfg: _       -> return coreCfg
    _invalidArgument -> fail "Error: wrong arguments, needed a path to the CoreConfig JSON configuration file\n"

main :: IO ()
main = do
  let path       = "."
      assetsPath = path </> "assets"
  createDirectoryIfMissing True assetsPath

  putStrLn "parsing Config ..."
  coreCfgPath <- parseArgs
  coreCfg     <- coreConfigIO coreCfgPath

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx coreCfg providers
    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx assetsPath
