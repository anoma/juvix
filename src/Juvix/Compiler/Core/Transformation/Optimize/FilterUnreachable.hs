module Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Transformation.Base

filterUnreachable :: InfoTable -> InfoTable
filterUnreachable tab =
  pruneInfoTable $
    over infoInductives goFilter $
      over infoIdentifiers goFilter tab
  where
    depInfo = createSymbolDependencyInfo tab

    goFilter :: HashMap Symbol a -> HashMap Symbol a
    goFilter =
      HashMap.filterWithKey (\sym _ -> isReachable depInfo sym)
