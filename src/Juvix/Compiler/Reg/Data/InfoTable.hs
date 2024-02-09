module Juvix.Compiler.Reg.Data.InfoTable
  ( module Juvix.Compiler.Reg.Data.InfoTable,
    module Juvix.Compiler.Tree.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Reg.Language
import Juvix.Compiler.Tree.Data.InfoTable.Base

type InfoTable = InfoTable' Code ()

type FunctionInfo = FunctionInfo' Code ()
