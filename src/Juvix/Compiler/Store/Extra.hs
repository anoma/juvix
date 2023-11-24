module Juvix.Compiler.Store.Extra where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable qualified as Core
import Juvix.Compiler.Store.Core.Extra
import Juvix.Compiler.Store.Language
import Juvix.Prelude

computeCombinedCoreInfoTable :: ModuleTable -> Core.InfoTable
computeCombinedCoreInfoTable mtab =
  mconcatMap (toCore . (^. moduleInfoCoreTable)) (HashMap.elems (mtab ^. moduleTable))
