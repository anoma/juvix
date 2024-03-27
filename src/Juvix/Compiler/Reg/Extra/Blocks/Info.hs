module Juvix.Compiler.Reg.Extra.Blocks.Info
  ( module Juvix.Compiler.Tree.Extra.Info,
    module Juvix.Compiler.Reg.Extra.Blocks.Info,
  )
where

import Juvix.Compiler.Reg.Data.Blocks.InfoTable
import Juvix.Compiler.Reg.Language.Base
import Juvix.Compiler.Tree.Extra.Info

data ExtraInfo = ExtraInfo
  { _extraInfoTable :: InfoTable,
    -- | Globally unique IDs for function symbols
    _extraInfoFUIDs :: HashMap Symbol Int,
    -- | IDs for constructor tags, consecutive starting from 0 for each
    -- inductive type separately
    _extraInfoCIDs :: HashMap Tag Int
  }

makeLenses ''ExtraInfo

computeExtraInfo :: InfoTable -> ExtraInfo
computeExtraInfo tab =
  ExtraInfo
    { _extraInfoTable = tab,
      _extraInfoFUIDs = computeFUIDs tab,
      _extraInfoCIDs = computeCIDs tab
    }
