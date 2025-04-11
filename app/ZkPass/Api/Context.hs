module ZkPass.Api.Context where

import           Data.Aeson
import qualified Data.Text                                   as T
import           GeniusYield.GYConfig                        (GYCoreConfig (..))
import           GeniusYield.Types
import           GHC.Generics
import           Prelude

import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)


------------------------- :Context & Setup: -------------------------                                                                                                                      

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
