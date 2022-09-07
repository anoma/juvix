module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.SizeInfo where

import Juvix.Compiler.Abstract.Extra
import Juvix.Prelude

-- | i = SizeInfo [v] ⇔ v is smaller than argument i of the caller function.
-- Indexes are 0 based
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
    ps = map (^. patternArgPattern) (filter (not . isBrace) args)
    isBrace :: PatternArg -> Bool
    isBrace = (== Implicit) . (^. patternArgIsImplicit)
    _sizeEqual = ps
    _sizeSmaller :: [[Pattern]]
    _sizeSmaller = map (^.. patternSubCosmos) ps
