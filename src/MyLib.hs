{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RankNTypes        #-}

module MyLib where

import           Prelude                                  (IO, putStrLn)

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
-- import           ZkFold.Symbolic.Data.ByteString          (ByteString (..))
-- import           ZkFold.Symbolic.Data.Bool                (Bool)
import           ZkFold.Symbolic.Class                    (Symbolic)
import           ZkFold.Symbolic.Cardano.Contracts.ZkPass

someFunc :: IO ()
someFunc = putStrLn "someFunc"

zkpr :: forall c. Symbolic c => ZKPassResult c
zkpr = ZKPassResult
       { allocatorAddress   = fromConstant (1000 :: Natural)
       , allocatorSignature = fromConstant (2000 :: Natural)
       , publicFields       = fromConstant (3000 :: Natural)
       , publicFieldsHash   = fromConstant (4000 :: Natural)
       , taskId             = fromConstant (5000 :: Natural)
       , uHash              = fromConstant (6000 :: Natural)
       , validatorAddress   = fromConstant (7000 :: Natural)
       , validatorSignature = fromConstant (8000 :: Natural)
       , publicKey          = fromConstant (9000 :: Natural)
       }

-- result :: forall c. Symbolic c => Bool c
-- result = zkPassSymbolicVerifier zkpr
