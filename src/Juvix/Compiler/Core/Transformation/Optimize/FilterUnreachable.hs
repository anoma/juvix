module Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Transformation.Base

filterUnreachable :: Module -> Module
filterUnreachable md =
  pruneInfoTable
    $ over (moduleInfoTable . infoInductives) goFilter
    $ over (moduleInfoTable . infoIdentifiers) goFilter md
  where
    depInfo = createSymbolDependencyInfo (md ^. moduleInfoTable)

    goFilter :: HashMap Symbol a -> HashMap Symbol a
    goFilter =
      HashMap.filterWithKey (\sym _ -> isReachable depInfo sym)
