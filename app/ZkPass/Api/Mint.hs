module ZkPass.Api.Mint where

import           Cardano.Api                            (AssetName (..),
                                                         parseAddressAny)
import           Control.Exception                      (throwIO)
import           Data.Aeson
import qualified Data.ByteString.Char8                  as BS8
import           Data.Coerce                            (coerce)
import qualified Data.Map.Strict                        as Map
import           Data.String                            (fromString)
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3                     (fromBuiltin, toBuiltin)
import           Prelude
import           Text.Parsec                            (parse)

import           ZkFold.Cardano.OffChain.Utils          (byteStringAsHex)
import qualified ZkFold.Cardano.OnChain.BLS12_381.F     as F
import           ZkFold.Cardano.OnChain.Utils           (dataToBlake)
import           ZkPass.Api.Context
import           ZkPass.Cardano.Example.IdentityCircuit (zkPassResultVerificationBytes)
import           ZkPass.Cardano.Example.ZkPassResult    (zkPassResult)
import           ZkPass.Cardano.UPLC.ZkPassToken        (zkPassTokenCompiled)
import           ZkPass.Utils                           (asTuple)


-- | Minting input parameters.
data MintInput = MintInput
  { miUsedAddrs       :: ![GYAddress]
  , miChangeAddr      :: !GYAddress
  , miBeneficiaryAddr :: !String
  , miScriptsTxOutRef :: !String
  , miResult          :: !(Maybe String)
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
      zkpr <- case miResult of
        Just res -> pure . toBuiltin $ BS8.pack res
        Nothing  -> zkPassResult

      let md = maybe Nothing (metadataMsg . fromString) miResult

      let (setup, input, proof) = zkPassResultVerificationBytes spX spPS . F.toInput $ dataToBlake zkpr
          zkPassTokenValidator  = validatorFromPlutus @PlutusV3 $ zkPassTokenCompiled setup
          zkPassPolicyId        = mintingPolicyId zkPassTokenValidator
          zkPassTokenName       = coerce @AssetName @GYTokenName . AssetName . fromBuiltin $ F.fromInput input
          zkPassToken           = GYToken zkPassPolicyId zkPassTokenName
          zkPassToken'          = asTuple zkPassToken
          tokens                = valueMake $ Map.singleton zkPassToken 1
          redeemer              = redeemerFromPlutusData proof

      let txOutRefSetup = txOutRefFromTuple (fromString miScriptsTxOutRef, 0)
          refScript     = GYMintReference @PlutusV3 txOutRefSetup zkPassTokenValidator
          skeleton      = mustHaveOutput (GYTxOut (addressFromApi benAddr) tokens Nothing Nothing)
                       <> mustMint refScript redeemer zkPassTokenName 1
                       <> mustHaveTxMetadata md

      txBody <- runGYTxBuilderMonadIO nid
                                      providers
                                      miUsedAddrs
                                      miChangeAddr
                                      Nothing
                                      (buildTxBody skeleton)

      return $ ZkPassResponse (byteStringAsHex $ fromBuiltin zkpr)
                              (fst zkPassToken')
                              (snd zkPassToken')
                              (unSignedTxWithFee txBody)

    Left err -> throwIO . userError . show $ err
