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

instance Semigroup FunctionInfoExtra where
  fi1 <> fi2 =
    FunctionInfoExtra
      { _functionMaxValueStackHeight = max (fi1 ^. functionMaxValueStackHeight) (fi2 ^. functionMaxValueStackHeight),
        _functionMaxTempStackHeight = max (fi1 ^. functionMaxTempStackHeight) (fi2 ^. functionMaxTempStackHeight)
      }
