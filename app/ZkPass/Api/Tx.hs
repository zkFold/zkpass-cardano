module ZkPass.Api.Tx where

import           Control.Monad      (void)
import           Data.Aeson
import           GeniusYield.Types
import           GHC.Generics
import           Prelude

import           ZkPass.Api.Context


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
