module Juvix.Compiler.Reg.Data.InfoTable
  ( module Juvix.Compiler.Reg.Data.InfoTable,
    module Juvix.Compiler.Tree.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Tree.Data.InfoTable.Base

newtype FunctionInfoExtra = FunctionInfoExtra
  { _functionLocalVarsNum :: Int
  }
  deriving stock (Eq)

makeLenses ''FunctionInfoExtra

type InfoTable = InfoTable' Code FunctionInfoExtra

type FunctionInfo = FunctionInfo' Code FunctionInfoExtra
