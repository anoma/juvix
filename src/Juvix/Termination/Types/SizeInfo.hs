module Juvix.Termination.Types.SizeInfo where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Prelude
import Juvix.Syntax.Abstract.Language.Extra

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

mkSizeInfo :: [Pattern] -> SizeInfo
mkSizeInfo ps = SizeInfo {..}
  where
    ps' = filter (not . isBrace) ps
    isBrace = \case
      PatternBraces {} -> True
      _ -> False
    _sizeEqual = ps
    _sizeSmaller =
      HashMap.fromList
        [ (v, i) | (i, p) <- zip [0 ..] ps', v <- smallerPatternVariables p
        ]
