module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.CheckerNew.Options where

import Juvix.Prelude hiding (fromEither)

data TypeCheckingMode
  = -- | The default mode with the normal behavious
    TypeCheckingNormal
  | -- | Used in `juvix dev instance-termination calls`.
    --
    -- 1. Does not attempt to resolve instane holes.
    --
    -- 2. Does not throw an error if instance-termination cannot be proved
    TypeCheckingBuildCallMap
  deriving stock (Eq, Show)

newtype TypeCheckingOptions = TypeCheckingOptions
  { _typeCheckingMode :: TypeCheckingMode
  }

defaultTypeCheckingOptions :: TypeCheckingOptions
defaultTypeCheckingOptions =
  TypeCheckingOptions
    { _typeCheckingMode = TypeCheckingNormal
    }

makeLenses ''TypeCheckingOptions
