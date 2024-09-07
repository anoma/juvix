module Juvix.Compiler.Internal.Data.Cast where

import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data CastType
  = CastInt
  | CastNat

data CastHole = CastHole
  { _castHoleHole :: Hole,
    _castHoleType :: CastType
  }

makeLenses ''CastHole

isCastInt :: CastType -> Bool
isCastInt = \case
  CastInt -> True
  CastNat -> False
