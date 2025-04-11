module ZkPass.Api.Setup where

import           Data.Aeson
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           Prelude
import           ZkFold.Cardano.OffChain.Utils          (currencySymbolOf)
import           ZkPass.Api.Context
import           ZkPass.Cardano.Example.IdentityCircuit (identityCircuitVerificationBytes)
import           ZkPass.Cardano.UPLC.ZkPassToken        (forwardingMintCompiled,
                                                         zkPassTokenCompiled)
import           ZkPass.Utils                           (fromAddrHex)

-- | Setup input parameters.
data SetupInput = SetupInput
  { ipUsedAddrs  :: ![GYAddress]
  , ipChangeAddr :: !GYAddress
  , ipTaskId     :: !Integer
  , ipZkpassAddr :: !String
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

-- | Setup response
data SetupResponse = SetupResponse
  { srPolicyId :: !String
  , srUnsigned :: !UnsignedTxResponse
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Handle to construct setup response.
handleSetup :: Ctx -> SetupParams -> SetupInput -> IO SetupResponse
handleSetup ctx SetupParams{..} SetupInput{..} = do
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

  let unsignedTxRes = unSignedTxWithFee txBody

  return $ SetupResponse policyId unsignedTxRes
