module Juvix.Data.Field where

import Data.FiniteField
import GHC.TypeNats
import Juvix.Prelude

type FField = Sigma Natural (TyCon PrimeField)

$(genSingletons [''Nat])

fadd :: FField -> FField -> FField
fadd x1 x2 = case (x1, x2) of
  (n1 :&: f1, n2 :&: f2) -> n1 :&: (f1 + f2)
