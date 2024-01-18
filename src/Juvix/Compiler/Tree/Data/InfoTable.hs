module Juvix.Compiler.Tree.Data.InfoTable
  ( module Juvix.Compiler.Tree.Data.InfoTable,
    module Juvix.Compiler.Tree.Data.InfoTable.Base,
  )
where

import Juvix.Compiler.Tree.Data.InfoTable.Base
import Juvix.Compiler.Tree.Language

type InfoTable = InfoTable' Node ()

type FunctionInfo = FunctionInfo' Node ()
