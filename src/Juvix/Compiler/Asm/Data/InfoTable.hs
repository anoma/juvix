module Juvix.Compiler.Asm.Data.InfoTable where

import Juvix.Compiler.Asm.Language

data InfoTable = InfoTable
  { _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoConstrs :: HashMap Tag ConstrInfo
  }

data FunctionInfo = FunctionInfo
  { _functionInfoName :: Name,
    _functionInfoSymbol :: Symbol,
    _functionInfoArgsNum :: Int,
    _functionInfoTempSize :: Int,
    _functionInfoStackSize :: Int,
    _functionInfoCode :: Code
  }

data ConstrInfo = ConstrInfo
  { _constrInfoName :: Name,
    _constrInfoTag :: Tag,
    _constrInfoArgsNum :: Int
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstrInfo

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoFunctions = mempty,
      _infoConstrs = mempty
    }
