module Juvix.Compiler.Reg.Data.Blocks.InfoTable
  ( module Juvix.Compiler.Reg.Data.Blocks.InfoTable,
    module Juvix.Compiler.Tree.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Reg.Language.Blocks
import Juvix.Compiler.Tree.Data.InfoTable.Base

type InfoTable = InfoTable' Block ()

type FunctionInfo = FunctionInfo' Block ()
