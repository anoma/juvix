module Juvix.Compiler.Asm.Data.InfoTable
  ( module Juvix.Compiler.Asm.Data.InfoTable,
    module Juvix.Compiler.Tree.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Tree.Data.InfoTable.Base

type InfoTable = InfoTable' Code (Maybe FunctionInfoExtra)

type FunctionInfo = FunctionInfo' Code (Maybe FunctionInfoExtra)

data FunctionInfoExtra = FunctionInfoExtra
  { _functionMaxValueStackHeight :: Int,
    _functionMaxTempStackHeight :: Int
  }

makeLenses ''FunctionInfoExtra
