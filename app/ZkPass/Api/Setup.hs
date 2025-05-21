module ZkPass.Api.Setup where

import           Data.Aeson
import qualified Data.ByteString.Lazy                   as BL
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           Prelude
import           System.Directory                       (doesFileExist)
import           System.FilePath                        ((</>))
import           Test.QuickCheck.Arbitrary              (Arbitrary (..))
import           Test.QuickCheck.Gen                    (generate)

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
  , ipFMTag      :: !Integer
  , ipZkpassAddr :: !String
  } deriving stock (Show, Generic)
    deriving anyclass FromJSON

-- | Output of setup execution
data SetupOutput = SetupOutput
  { soPolicyId :: !String
  , soUnsigned :: !UnsignedTxResponse
  } deriving stock (Show, Generic)
    deriving anyclass ToJSON

-- | Setup response
data SetupResponse = SetupResponse { srOut :: !(Maybe SetupOutput) }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

-- | Handle to construct setup response.
handleSetup :: Ctx -> FilePath -> SetupInput -> IO SetupResponse
handleSetup ctx path SetupInput{..} = do
  let nid       = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
      setupFile = path </> "setup-params.json"

  setupFileExists <- doesFileExist setupFile

  if setupFileExists
    then return $ SetupResponse Nothing
    else do
      x  <- generate arbitrary
      ps <- generate arbitrary

      let partialSetupParams = SetupParams x ipFMTag Nothing
      BL.writeFile setupFile $ encode partialSetupParams

      let (setup, _, _) = identityCircuitVerificationBytes x ps

      let plutusValidators = [zkPassTokenCompiled setup, forwardingMintCompiled ipFMTag]
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

      return . SetupResponse . Just $ SetupOutput policyId unsignedTxRes
