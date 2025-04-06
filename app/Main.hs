{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Cardano.Api                 (parseAddressAny)
import           Control.Exception           (try)
import           Control.Monad               (void)
import           Control.Monad.Trans.Except  (ExceptT(..))
import           Data.Aeson
import qualified Data.Text                   as T
import           GeniusYield.GYConfig        (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.TxBuilder
import           GeniusYield.Types           (GYAddress, GYProviders (..), GYTx, GYTxBody, GYTxId, GYTxOut (..), GYTxWitness, PlutusVersion (..))
import           GeniusYield.Types.Address   (addressFromApi)
import           GeniusYield.Types.Script    (GYAnyScript (..), validatorFromPlutus)
import           GeniusYield.Types.TxBody    (getTxBody, makeSignedTransaction, txBodyFee, txBodyTxId, unsignedTx)
import           GeniusYield.Types.Tx        (txToHex)
import           GHC.Generics
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Prelude
import           Servant
import           System.Environment          (getArgs)
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))
import           Test.QuickCheck.Gen         (generate)
import           Text.Parsec                 (parse)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Cardano.OffChain.Utils               (currencySymbolOf)
import           ZkPass.Cardano.Example.IdentityCircuit      (identityCircuitVerificationBytes)
import           ZkPass.Cardano.UPLC.ZkPassToken             (forwardingMintCompiled, zkPassTokenCompiled)
                 
-- | Configuration context.
data Ctx = Ctx
  { ctxCoreCfg   :: !GYCoreConfig
  , ctxProviders :: !GYProviders
  }

-- | Setup parameters.
data SetupParams = SetupParams
  { spX  :: !Fr
  , spPS :: !(PlonkupProverSecret BLS12_381_G1)
  }

-- | Input parameters.
data InputParams = InputParams
  { ipUsedAddrs  :: ![GYAddress]
  , ipChangeAddr :: !GYAddress
  , ipTaskId     :: !Integer
  , ipZkpassAddr :: !String
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

-- | System parameters.
data SystemParams = SystemParams
  { sysTaskId   :: !Integer
  , sysPolicyId :: !String
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Return type for ZkPassAPI endpoint.
data UnsignedTxResponse = UnsignedTxResponse
  { urspTxBodyHex :: !T.Text              -- ^ Unsigned transaction cbor.
  , urspTxFee     :: !(Maybe Integer)     -- ^ Tx fees.
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Setup response
data SetupResponse = SetupResponse
  { srParams   :: !SystemParams
  , srUnsigned :: !UnsignedTxResponse
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Derive GYAddress
fromAddrHex :: String -> GYAddress
fromAddrHex addrHex = case parse parseAddressAny "" addrHex of
  Right addr -> addressFromApi addr
  Left err   -> error $ show err

-- | Construct `UnsignedTxResponse` return type for our endpoint given the transaction body.
unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody = UnsignedTxResponse
  { urspTxBodyHex  = T.pack . txToHex $ unsignedTx txBody
  , urspTxFee      = Just $ txBodyFee txBody
  }

-- | Construct setup response.
setupResponse :: Ctx -> SetupParams -> InputParams -> IO SetupResponse
setupResponse ctx SetupParams{..} InputParams{..} = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx

  let (setup, _, _) = identityCircuitVerificationBytes spX spPS

  let plutusValidators = [zkPassTokenCompiled setup, forwardingMintCompiled ipTaskId]
      policyId         = show $ currencySymbolOf $ head plutusValidators
      validators       = GYPlutusScript . validatorFromPlutus @'PlutusV3 <$> plutusValidators

  let valTxOuts = GYTxOut (fromAddrHex ipZkpassAddr) mempty Nothing . Just <$> validators
      skeleton  = mconcat $ mustHaveOutput <$> valTxOuts

  txBody <- runGYTxBuilderMonadIO nid
                                  providers
                                  ipUsedAddrs
                                  ipChangeAddr
                                  Nothing
                                  (buildTxBody skeleton)

  let systemParams   = SystemParams ipTaskId policyId
      unsignedTxRes = unSignedTxWithFee txBody

  return $ SetupResponse systemParams unsignedTxRes

-- | Handle task
handleTask :: Ctx -> SetupParams -> InputParams -> IO SetupResponse
handleTask = setupResponse

-- | Submit parameters to add for witness.
data AddWitAndSubmitParams = AddWitAndSubmitParams
  { awasTxUnsigned :: !GYTx
  , awasTxWit      :: !GYTxWitness
  } deriving stock Generic
    deriving anyclass FromJSON

-- | Return type of API when submitting a transaction.
data SubmitTxResponse = SubmitTxResponse
  { submitTxFee :: !Integer
  , submitTxId  :: !GYTxId
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Construct `SubmitTxResponse` return type from the given signed transaction body.
txBodySubmitTxResponse :: GYTxBody -> SubmitTxResponse
txBodySubmitTxResponse txBody = SubmitTxResponse
  { submitTxFee = txBodyFee txBody
  , submitTxId  = txBodyTxId txBody
  }

-- | Handle for adding key witness to the unsigned transaction & then submit it.
handleAddWitAndSubmitTx :: Ctx -> AddWitAndSubmitParams -> IO SubmitTxResponse
handleAddWitAndSubmitTx Ctx{..} AddWitAndSubmitParams{..} = do
  let txBody = getTxBody awasTxUnsigned
  void . gySubmitTx ctxProviders $ makeSignedTransaction awasTxWit txBody
  return $ txBodySubmitTxResponse txBody

-- | Type for our Servant API.
type API = "task" :> ReqBody '[JSON] InputParams
                  :> Post    '[JSON] SetupResponse
      :<|> "add-wit-and-submit" :> ReqBody '[JSON] AddWitAndSubmitParams
                                :> Post    '[JSON] SubmitTxResponse

-- | Server Handler
server :: Ctx -> SetupParams -> ServerT API IO
server ctx sp = handleTask ctx sp
           :<|> handleAddWitAndSubmitTx ctx

appApi :: Proxy API
appApi = Proxy

app :: Ctx -> SetupParams -> Application
app ctx sp = cors (const $ Just simpleCorsResourcePolicy { corsRequestHeaders = [HttpTypes.hContentType] }) $
  serve appApi $ hoistServer appApi (Handler . ExceptT . try) $
  server ctx sp

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

  x  <- generate arbitrary
  ps <- generate arbitrary
  let setupParams = SetupParams x ps

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx coreCfg providers
    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx setupParams
