module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.SizeInfo where

import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

-- | i = SizeInfo [v] ⇔ v is smaller than argument i of the caller function.
-- The first (leftmost) argument has index 0
newtype SizeInfo = SizeInfo
  { _sizeEqual :: [Pattern]
  }

makeLenses ''SizeInfo

emptySizeInfo :: SizeInfo
emptySizeInfo =
  SizeInfo
    { _sizeEqual = mempty
    }

mkSizeInfo :: [PatternArg] -> SizeInfo
mkSizeInfo args = SizeInfo ps
  where
    ps :: [Pattern]
    ps = map (^. patternArgPattern) (filter (not . isImplicit') args)

    isImplicit' :: PatternArg -> Bool
    isImplicit' = isImplicitOrInstance . (^. patternArgIsImplicit)
