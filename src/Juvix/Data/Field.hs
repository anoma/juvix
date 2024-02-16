module Juvix.Data.Field where

import Data.Singletons.Decide
import GHC.TypeLits.Singletons ()
import Juvix.Data.FiniteField.PrimeField
import Juvix.Prelude

type FField = Sigma Natural (TyCon1 PrimeField)

fadd :: FField -> FField -> FField
fadd
  ((n1 :: Sing (p :: Natural)) :&: (f1 :: PrimeField p))
  ((n2 :: Sing (r :: Natural)) :&: (f2 :: PrimeField r)) =
    let x = decideEquality n1 n2
     in case x :: Maybe (p :~: r) of
          Just Refl -> n1 :&: withSingI n1 (f1 + f2)
          Nothing -> impossible
