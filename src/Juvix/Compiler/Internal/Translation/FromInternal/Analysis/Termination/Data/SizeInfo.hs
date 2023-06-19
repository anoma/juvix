module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Data.SizeInfo where

import Juvix.Compiler.Internal.Extra
import Juvix.Prelude

-- | i = SizeInfo [v] ⇔ v is smaller than argument i of the caller function.
-- The first (leftmost) argument has index 0
data SizeInfo = SizeInfo
  { _sizeSmaller :: [[Pattern]],
    _sizeEqual :: [Pattern]
  }

makeLenses ''SizeInfo

emptySizeInfo :: SizeInfo
emptySizeInfo =
  SizeInfo
    { _sizeEqual = mempty,
      _sizeSmaller = mempty
    }

mkSizeInfo :: [PatternArg] -> SizeInfo
mkSizeInfo args = SizeInfo {..}
  where
    ps :: [Pattern]
    ps = map (^. patternArgPattern) (filter (not . isImplicit') args)
    isImplicit' :: PatternArg -> Bool
    isImplicit' = (== Implicit) . (^. patternArgIsImplicit)
    _sizeEqual = ps
    _sizeSmaller :: [[Pattern]]
    _sizeSmaller = map (^.. patternSubCosmos) ps
