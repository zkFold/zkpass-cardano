{-# LANGUAGE TypeOperators #-}

module ZkPass.Cardano.Example.IdentityCircuit where

import           Data.Aeson                                  (FromJSON, ToJSON)
import           GHC.Generics                                (Generic,
                                                              Par1 (..),
                                                              U1 (..),
                                                              type (:*:) (..))
import           Prelude                                     hiding (Bool,
                                                              Eq (..),
                                                              Fractional (..),
                                                              Num (..), length)

import           ZkFold.Base.Algebra.Basic.Class             (zero)
import           ZkFold.Base.Algebra.Basic.Field             (toZp)
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1, Fr)
import           ZkFold.Base.Protocol.NonInteractiveProof    (HaskellCore,
                                                              NonInteractiveProof (..))
import           ZkFold.Base.Protocol.Plonkup                (Plonkup (..))
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret  (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (..))
import           ZkFold.Cardano.OffChain.Plonkup             (PlonkupN, mkInput,
                                                              mkProof, mkSetup)
import           ZkFold.Cardano.OnChain.BLS12_381.F          (F (..))
import           ZkFold.Cardano.OnChain.Plonkup              ()
import           ZkFold.Cardano.OnChain.Plonkup.Data         (InputBytes,
                                                              ProofBytes,
                                                              SetupBytes)
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..),
                                                              eval, idCircuit)


data IdentityCircuitContract = IdentityCircuitContract
  { x  :: Fr
  , ps :: PlonkupProverSecret BLS12_381_G1
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

identityCircuit :: ArithmeticCircuit Fr (U1 :*: U1) Par1 Par1
identityCircuit = idCircuit

identityCircuitVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> (SetupBytes, InputBytes, ProofBytes)
identityCircuitVerificationBytes x ps =
    let (omega, k1, k2) = getParams 2
        witnessInputs   = eval identityCircuit (U1 :*: U1) $ Par1 zero
        plonkup         = Plonkup omega k1 k2 identityCircuit x :: PlonkupN (U1 :*: U1) Par1 2
        setupP          = setupProve @_ @HaskellCore plonkup
        setupV          = setupVerify @_ @HaskellCore plonkup
        witness         = (PlonkupWitnessInput @_ @_ @BLS12_381_G1 (U1 :*: U1) witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN (U1 :*: U1) Par1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)

zkPassResultVerificationBytes :: Fr -> PlonkupProverSecret BLS12_381_G1 -> F -> (SetupBytes, InputBytes, ProofBytes)
zkPassResultVerificationBytes x ps state =
    let F n             = state
        state'          = toZp n :: Fr
        (omega, k1, k2) = getParams 2
        witnessInputs   = Par1 state'
        plonkup         = Plonkup omega k1 k2 identityCircuit x :: PlonkupN (U1 :*: U1) Par1 2
        setupP          = setupProve @_ @HaskellCore plonkup
        setupV          = setupVerify @_ @HaskellCore plonkup
        witness         = (PlonkupWitnessInput (U1 :*: U1) witnessInputs, ps)
        (input, proof)  = prove @(PlonkupN (U1 :*: U1) Par1 2) @HaskellCore setupP witness

    in (mkSetup setupV, mkInput input, mkProof proof)
