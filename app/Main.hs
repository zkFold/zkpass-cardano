{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Cardano.Api                 (AssetName (..), parseAddressAny)
import           Control.Exception           (throwIO, try)
import           Control.Monad               (void)
import           Control.Monad.Trans.Except  (ExceptT(..))
import           Data.Aeson
import qualified Data.ByteString.Lazy        as BL
import           Data.Coerce                 (coerce)
import qualified Data.Map.Strict             as Map
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import           GeniusYield.GYConfig        (GYCoreConfig (..), coreConfigIO, withCfgProviders)
import           GeniusYield.Transaction.Common (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import qualified Network.HTTP.Types          as HttpTypes
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           PlutusLedgerApi.V3          (fromBuiltin, toBuiltinData)
import           Prelude
import           Servant
import           System.Directory            (createDirectoryIfMissing)
import           System.Environment          (getArgs)
import           System.FilePath             ((</>))
import           System.IO                   (withFile, IOMode(AppendMode))
import           Test.QuickCheck.Arbitrary   (Arbitrary (..))
import           Test.QuickCheck.Gen         (generate)
import           Text.Parsec                 (parse)

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Cardano.OffChain.Utils               (byteStringAsHex, currencySymbolOf)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F          as F
import           ZkFold.Cardano.OnChain.Plonkup.Data         (ProofBytes (..))
import           ZkPass.Cardano.Example.IdentityCircuit      (identityCircuitVerificationBytes, zkPassResultVerificationBytes)
import           ZkPass.Cardano.Example.ZkPassResult         (zkPassResult)
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
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

------------------------- :Setup: -------------------------

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

-- | Setup response
data SetupResponse = SetupResponse
  { srParams   :: !SystemParams
  , srUnsigned :: !UnsignedTxResponse
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Construct setup response.
setupResponse :: Ctx -> SetupParams -> InputParams -> IO SetupResponse
setupResponse ctx SetupParams{..} InputParams{..} = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx

  let (setup, _, _) = identityCircuitVerificationBytes spX spPS

  let plutusValidators = [zkPassTokenCompiled setup, forwardingMintCompiled ipTaskId]
      policyId         = show $ currencySymbolOf $ head plutusValidators
      validators       = GYPlutusScript . validatorFromPlutus @PlutusV3 <$> plutusValidators

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

-- | Handle setup
handleSetup :: Ctx -> SetupParams -> InputParams -> IO SetupResponse
handleSetup = setupResponse

------------------------- :Transfer: -------------------------

handleTransfer :: Ctx -> TransferInput -> IO UnsignedTxResponse
handleTransfer Ctx{..} TransferInput{..} = do
  let nid       = cfgNetworkId ctxCoreCfg
      providers = ctxProviders

  let forwardingMintValidator = validatorFromPlutus @PlutusV3 $ forwardingMintCompiled tiTaskId
      forwardingMintAddr      = addressFromValidator nid forwardingMintValidator

  let cs          = mintingPolicyIdToCurrencySymbol . fromString $ tiPolicyId
      inlineDatum = Just (datumFromPlutusData cs, GYTxOutUseInlineDatum @PlutusV3)

  params <- gyGetProtocolParameters providers
  let outMin          = GYTxOut forwardingMintAddr (valueFromLovelace 0) inlineDatum Nothing
      minUtxoLovelace = toInteger $ minimumUTxO params outMin

  if tiReward >= minUtxoLovelace
    then do
      let rewardValue = valueFromLovelace tiReward
          skeleton    = mustHaveOutput (GYTxOut forwardingMintAddr rewardValue inlineDatum Nothing)

      txBody <- runGYTxBuilderMonadIO nid
                                      providers
                                      tiUsedAddrs
                                      tiChangeAddr
                                      Nothing
                                      (buildTxBody skeleton)

      return $ unSignedTxWithFee txBody

    else throwIO $ userError "Reward must be at least minimumUTxO lovelace."

------------------------- :Minting ZkPass Token: -------------------------

-- | Minting input parameters.
data MintInput = MintInput
  { miUsedAddrs       :: ![GYAddress]
  , miChangeAddr      :: !GYAddress
  , miBeneficiaryAddr :: !String
  , miScriptsTxOutRef :: !String
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

-- | ZkPass response parameters.
data ZkPassResponse = ZkPassResponse
  { zkprResult   :: !String
  , zkprPolicyId :: !String
  , zkprTknName  :: !String
  , zkprUnsigned :: !UnsignedTxResponse
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

handleMint :: Ctx -> SetupParams -> MintInput -> IO ZkPassResponse
handleMint Ctx{..} SetupParams{..} MintInput{..} = do
  let nid       = cfgNetworkId ctxCoreCfg
      providers = ctxProviders

  case parse parseAddressAny "" miBeneficiaryAddr of
    Right benAddr -> do
      zkpr <- zkPassResult

      let (setup, input, proof) = zkPassResultVerificationBytes spX spPS $ F.toInput zkpr
          zkPassTokenValidator  = validatorFromPlutus @PlutusV3 $ zkPassTokenCompiled setup
          zkPassPolicyId        = mintingPolicyId zkPassTokenValidator
          zkPassTokenName       = coerce @AssetName @GYTokenName . AssetName . fromBuiltin $ F.fromInput input
          zkPassToken           = GYToken zkPassPolicyId zkPassTokenName
          tokens                = valueMake $ Map.singleton zkPassToken 1
          redeemer              = redeemerFromPlutusData proof

      let txOutRefSetup = txOutRefFromTuple (fromString miScriptsTxOutRef, 0)
          refScript     = GYMintReference @PlutusV3 txOutRefSetup zkPassTokenValidator
          skeleton      = mustHaveOutput (GYTxOut (addressFromApi benAddr) tokens Nothing Nothing)
                       <> mustMint refScript redeemer zkPassTokenName 1

      txBody <- runGYTxBuilderMonadIO nid
                                      providers
                                      miUsedAddrs
                                      miChangeAddr
                                      Nothing
                                      (buildTxBody skeleton)

      return $ ZkPassResponse (byteStringAsHex $ fromBuiltin zkpr)
                              (fst $ asTuple zkPassToken)
                              (snd $ asTuple zkPassToken)
                              (unSignedTxWithFee txBody)

    Left err      -> throwIO . userError . show $ err

------------------------- :Burning: -------------------------

-- | Burning input parameters.
data BurnInput = BurnInput
  { biUsedAddrs       :: ![GYAddress]
  , biChangeAddr      :: !GYAddress
  , biTaskId          :: !Integer
  , biZkPassToken     :: !String
  , biScriptsTxOutRef :: !String
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

handleBurn :: Ctx -> SetupParams -> BurnInput -> IO UnsignedTxResponse
handleBurn Ctx{..} SetupParams{..} BurnInput{..} = do
  let nid       = cfgNetworkId ctxCoreCfg
      providers = ctxProviders

  let forwardingMintValidator = validatorFromPlutus @PlutusV3 $ forwardingMintCompiled biTaskId
      forwardingMintAddr      = addressFromValidator nid forwardingMintValidator

  let zkpassAsset = fromString biZkPassToken :: GYAssetClass

  case zkpassAsset of
    GYToken zkpPolicy zkpTokenName -> do
      let cs          = mintingPolicyIdToCurrencySymbol zkpPolicy
          inlineDatum = GYOutDatumInline $ datumFromPlutusData cs

      utxosAtFM <- runGYTxQueryMonadIO nid
                                       providers
                                       (utxosAtAddress forwardingMintAddr (Just GYLovelace))

      let utxosAtFMList = utxosToList $ filterUTxOs (\u -> utxoOutDatum u == inlineDatum) utxosAtFM

      case utxosAtFMList of
        [utxoAtFM] -> do
          let (setup, _, _)        = identityCircuitVerificationBytes spX spPS
              zkPassTokenValidator = validatorFromPlutus @PlutusV3 $ zkPassTokenCompiled setup

          let setupTxOutRef   = txOutRefFromTuple (fromString biScriptsTxOutRef, 0)
              forwardTxOutRef = txOutRefFromTuple (fromString biScriptsTxOutRef, 1)

          let setupRef   = GYBuildPlutusScriptReference @PlutusV3 setupTxOutRef zkPassTokenValidator
              forwardRef = GYBuildPlutusScriptReference @PlutusV3 forwardTxOutRef forwardingMintValidator
              forwardWit = GYTxInWitnessScript forwardRef Nothing unitRedeemer

          let dummyRedeemer' = ProofBytes "" "" "" "" "" "" "" "" "" "" "" "" "" 0 0 0 0 0 0 0 0 0 0 0 0 (F.F 0)
              dummyRedeemer  = redeemerFromPlutusData $ toBuiltinData dummyRedeemer'

          let skeleton = mustHaveInput (GYTxIn @PlutusV3 (utxoRef utxoAtFM) forwardWit)
                         <> mustMint (GYBuildPlutusScript setupRef) dummyRedeemer zkpTokenName (-1)

          txBody <- runGYTxBuilderMonadIO nid
                                          providers
                                          biUsedAddrs
                                          biChangeAddr
                                          Nothing
                                          $ do
            ownAddrs <- ownAddresses
            let skeleton' = skeleton <> mustHaveOutput (GYTxOut (head ownAddrs) (utxoValue utxoAtFM) Nothing Nothing)
            buildTxBody skeleton'

          return $ unSignedTxWithFee txBody

        _ -> throwIO $ userError "No UTxO with expected datum found."
    _ -> throwIO $ userError "Missing native token specification."

------------------------- :Unsigned response: -------------------------

data UnsignedTxResponse = UnsignedTxResponse
  { urspTxBodyHex :: !T.Text              -- ^ Unsigned transaction cbor.
  , urspTxFee     :: !(Maybe Integer)     -- ^ Tx fees.
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Construct `UnsignedTxResponse` return type for our endpoint given the transaction body.
unSignedTxWithFee :: GYTxBody -> UnsignedTxResponse
unSignedTxWithFee txBody = UnsignedTxResponse
  { urspTxBodyHex  = T.pack . txToHex $ unsignedTx txBody
  , urspTxFee      = Just $ txBodyFee txBody
  }

------------------------- :Sign & Submit: -------------------------

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

-- | Transfer input parameters.
data TransferInput = TransferInput
  { tiUsedAddrs  :: ![GYAddress]
  , tiChangeAddr :: !GYAddress
  , tiTaskId     :: !Integer
  , tiPolicyId   :: !String
  , tiReward     :: !Integer
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

------------------------- :own address: -------------------------

-- | Own addresses input.
data OwnAddresses = OwnAddresses { oaUsedAddrs :: ![GYAddress] }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Return own address as text.
data OwnAddress = OwnAddress { oaOwnAddress :: !T.Text }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- | Handle to get own address.
handleOwnAddr :: OwnAddresses -> IO OwnAddress
handleOwnAddr OwnAddresses{..} = pure . OwnAddress . addressToText $ head oaUsedAddrs

------------------------- :Server: -------------------------

-- | Type for our Servant API.
type API = "setup" :> ReqBody '[JSON] InputParams
                   :> Post    '[JSON] SetupResponse
      :<|> "transfer" :> ReqBody '[JSON] TransferInput
                      :> Post    '[JSON] UnsignedTxResponse
      :<|> "mint" :> ReqBody '[JSON] MintInput
                  :> Post    '[JSON] ZkPassResponse
      :<|> "burn" :> ReqBody '[JSON] BurnInput
                  :> Post    '[JSON] UnsignedTxResponse
      :<|> "add-wit-and-submit" :> ReqBody '[JSON] AddWitAndSubmitParams
                                :> Post    '[JSON] SubmitTxResponse
      :<|> "own-addr" :> ReqBody '[JSON] OwnAddresses
                      :> Post    '[JSON] OwnAddress

-- | Server Handler
server :: Ctx -> SetupParams -> ServerT API IO
server ctx sp = handleSetup ctx sp
           :<|> handleTransfer ctx
           :<|> handleMint ctx sp
           :<|> handleBurn ctx sp
           :<|> handleAddWitAndSubmitTx ctx
           :<|> handleOwnAddr

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

  createDirectoryIfMissing True "./log"
  withFile ("log" </> "plonkup-raw-contract-data.json") AppendMode $ \h -> BL.hPut h (encode setupParams)

  putStrLn "Loading Providers ..."
  withCfgProviders coreCfg "api-server" $ \providers -> do
    let port = 8080
        ctx  = Ctx coreCfg providers
    putStrLn $ "Serving on http://localhost:" ++ show port
    run port $ app ctx setupParams



------------------------- :Helper functions: -------------------------

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

