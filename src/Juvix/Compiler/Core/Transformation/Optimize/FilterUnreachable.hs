module Juvix.Compiler.Core.Transformation.Optimize.FilterUnreachable where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Transformation.Base

filterUnreachable :: InfoTable -> InfoTable
filterUnreachable tab = pruneInfoTable $ over infoIdentifiers goFilter tab
  where
    depInfo = createIdentDependencyInfo tab

    goFilter :: HashMap Symbol IdentifierInfo -> HashMap Symbol IdentifierInfo
    goFilter idents =
      HashMap.filterWithKey (\sym _ -> isReachable depInfo sym) idents
