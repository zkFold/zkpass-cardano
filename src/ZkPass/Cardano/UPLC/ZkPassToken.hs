{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module ZkPass.Cardano.UPLC.ZkPassToken where

import           PlutusLedgerApi.V3
import           PlutusTx                                 (CompiledCode,
                                                           compile, liftCodeDef,
                                                           unsafeApplyCode)
import           PlutusTx.AssocMap                        (keys)
import qualified PlutusTx.Builtins.Internal               as BI
import           PlutusTx.Prelude                         (Bool (..),
                                                           BuiltinUnit, Integer,
                                                           Maybe (..), Ord (..),
                                                           check, error, find,
                                                           isJust, ($), (&&),
                                                           (.), (||))

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore,
                                                           NonInteractiveProof (..))
import           ZkFold.Cardano.OnChain.BLS12_381.F       (toInput)
import           ZkFold.Cardano.OnChain.Plonkup           (PlonkupPlutus)
import           ZkFold.Cardano.OnChain.Plonkup.Data      (ProofBytes,
                                                           SetupBytes)
import           ZkFold.Cardano.OnChain.Utils             (ScriptLabel,
                                                           eqMintingPurpose)


-- | Plutus script (minting policy) for verifying zkpass computations on-chain.
--
-- The zkpass token is minted if and only if the Plonkup `proof` is valid for the `computation` on the `input`
-- derived from the token name.  The computation is encoded into the token's currency symbol (aka policyID).
{-# INLINABLE untypedZkPassToken #-}
untypedZkPassToken :: SetupBytes -> BuiltinData -> BuiltinUnit
untypedZkPassToken computation ctx =
  check $ eqCs && (conditionBurning || conditionVerifying)
  where
      scriptContextTxInfo'     = BI.snd $ BI.unsafeDataAsConstr ctx
      scriptContextRedeemer'   = BI.tail scriptContextTxInfo'
      scriptContextScriptInfo' = BI.tail scriptContextRedeemer'

      cs'        = BI.head $ BI.snd $ BI.unsafeDataAsConstr $ BI.head scriptContextScriptInfo'
      info       = BI.head scriptContextTxInfo'
      txInfoMint = BI.head $ tail4 $ BI.snd $ BI.unsafeDataAsConstr info
      tail4      = BI.tail . BI.tail . BI.tail . BI.tail

      mints' = BI.head $ BI.unsafeDataAsMap txInfoMint
      eqCs   = BI.ifThenElse (BI.equalsData cs' $ BI.fst mints') True False
      m'     = BI.head $ BI.unsafeDataAsMap $ BI.snd mints'

      t = BI.unsafeDataAsB $ BI.fst m'
      n = BI.unsafeDataAsI $ BI.snd m'

      -- Extract redeemer from ScriptContext
      proof = unsafeFromBuiltinData @ProofBytes $ BI.head scriptContextRedeemer'

      -- Computing public input from the token name
      input = toInput t

      -- Burning already minted tokens
      conditionBurning = n < 0

      -- Verifying the Plonkup `proof` for the `computation` on `input`
      conditionVerifying = verify @PlonkupPlutus @HaskellCore computation input proof

zkPassTokenCompiled :: SetupBytes -> CompiledCode (BuiltinData -> BuiltinUnit)
zkPassTokenCompiled computation =
    $$(compile [|| untypedZkPassToken ||])
    `unsafeApplyCode` liftCodeDef computation

-- | The Plutus spending script that forwards verification to a minting script.
{-# INLINABLE forwardingMint #-}
forwardingMint :: ScriptLabel -> BuiltinByteString -> () -> ScriptContext -> Bool
forwardingMint _label symbolHash _ ctx =
    -- Searching for the minting script with a specific hash
    isJust $ find (eqMintingPurpose sc) reds
    where
        -- Constructing the minting script purpose
        sc = CurrencySymbol symbolHash

        -- Finding scripts to be executed
        reds = keys $ txInfoRedeemers $ scriptContextTxInfo ctx

{-# INLINABLE untypedForwardingMint #-}
untypedForwardingMint :: ScriptLabel -> BuiltinData -> BuiltinUnit
untypedForwardingMint label' ctx' =
  let ctx = unsafeFromBuiltinData ctx' in
    case scriptContextScriptInfo ctx of
      SpendingScript _ (Just dat) -> check $
        forwardingMint
          label'
          (unsafeFromBuiltinData . getDatum $ dat)
          (unsafeFromBuiltinData . getRedeemer . scriptContextRedeemer $ ctx)
          ctx
      _                           -> error ()

forwardingMintCompiled :: Integer -> CompiledCode (BuiltinData -> BuiltinUnit)
forwardingMintCompiled label =
    $$(compile [|| untypedForwardingMint ||])
    `unsafeApplyCode` liftCodeDef label
