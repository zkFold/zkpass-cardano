module ZkPass.Api.Transfer where

import           Control.Exception               (throwIO)
import           Data.Aeson
import           Data.String                     (fromString)
import           GeniusYield.GYConfig            (GYCoreConfig (..))
import           GeniusYield.Transaction.Common  (minimumUTxO)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           Prelude

import           ZkPass.Cardano.UPLC.ZkPassToken (forwardingMintCompiled)
import           ZkPass.Api.Context


-- | Transfer input parameters.
data TransferInput = TransferInput
  { tiUsedAddrs  :: ![GYAddress]
  , tiChangeAddr :: !GYAddress
  , tiTaskId     :: !Integer
  , tiPolicyId   :: !String
  , tiReward     :: !Integer
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

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
