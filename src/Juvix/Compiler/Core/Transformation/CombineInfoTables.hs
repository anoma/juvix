module Juvix.Compiler.Core.Transformation.CombineInfoTables where

import Juvix.Compiler.Core.Transformation.Base

combineInfoTables :: Module -> Module
combineInfoTables md =
  md
    { _moduleInfoTable = computeCombinedInfoTable md,
      _moduleImportsTable = mempty
    }
