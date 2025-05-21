module ZkPass.Api.Transfer where

import           Control.Exception               (throwIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy            as BL
import           Data.Maybe                      (fromJust)
import           Data.String                     (fromString)
import           GeniusYield.GYConfig            (GYCoreConfig (..))
import           GeniusYield.Transaction.Common  (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           Prelude
import           System.FilePath                 ((</>))

import           ZkPass.Api.Context
import           ZkPass.Cardano.UPLC.ZkPassToken (forwardingMintCompiled)


-- | Transfer input parameters.
data TransferInput = TransferInput
  { tiUsedAddrs  :: ![GYAddress]
  , tiChangeAddr :: !GYAddress
  , tiPolicyId   :: !String
  , tiReward     :: !Integer
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

handleTransfer :: Ctx -> FilePath -> TransferInput -> IO UnsignedTxResponse
handleTransfer Ctx{..} path TransferInput{..} = do
  SetupParams _ fmTag _ <- fromJust . decode <$> BL.readFile (path </> "setup-params.json")

  let nid       = cfgNetworkId ctxCoreCfg
      providers = ctxProviders

  let forwardingMintValidator = validatorFromPlutus @PlutusV3 $ forwardingMintCompiled fmTag
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
