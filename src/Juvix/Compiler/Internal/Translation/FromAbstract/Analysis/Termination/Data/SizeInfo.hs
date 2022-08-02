module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Data.SizeInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Abstract.Extra
import Juvix.Prelude

-- | i = SizeInfo [v] ⇔ v is smaller than argument i of the caller function.
-- Indexes are 0 based
data SizeInfo = SizeInfo
  { _sizeSmaller :: HashMap VarName Int,
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
mkSizeInfo ps = SizeInfo {..}
  where
    ps' :: [Pattern]
    ps' = map (^. patternArgPattern) (filter (not . isBrace) ps)
    isBrace :: PatternArg -> Bool
    isBrace = (== Implicit) . (^. patternArgIsImplicit)
    _sizeEqual = map (^. patternArgPattern) ps
    _sizeSmaller :: HashMap VarName Int
    _sizeSmaller =
      HashMap.fromList
        [ (v, i) | (i, p) <- zip [0 ..] ps', v <- p ^.. smallerPatternVariables
        ]
