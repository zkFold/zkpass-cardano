module ZkPass.Api.Burn where

import           Control.Exception                      (throwIO)
import           Data.Aeson
import           Data.String                            (fromString)
import           GeniusYield.GYConfig                   (GYCoreConfig (..))
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           GHC.Generics
import           PlutusLedgerApi.V3                     (toBuiltinData)
import           Prelude
import qualified ZkFold.Cardano.OnChain.BLS12_381.F     as F
import           ZkFold.Cardano.OnChain.Plonkup.Data    (ProofBytes (..))
import           ZkPass.Cardano.Example.IdentityCircuit (identityCircuitVerificationBytes)

import           ZkPass.Api.Context
import           ZkPass.Cardano.UPLC.ZkPassToken        (forwardingMintCompiled,
                                                         zkPassTokenCompiled)


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
