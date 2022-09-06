module Juvix.Compiler.Asm.Data.InfoTable where

import Juvix.Compiler.Asm.Language

data InfoTable = InfoTable
  { _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoConstrs :: HashMap Tag ConstructorInfo,
    _infoInductives :: HashMap Symbol InductiveInfo
  }

data FunctionInfo = FunctionInfo
  { _functionName :: Maybe Name,
    _functionSymbol :: Symbol,
    _functionArgsNum :: Int,
    _functionType :: Type,
    _functionCode :: Code
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Maybe Name,
    _constructorTag :: Tag,
    _constructorArgsNum :: Int,
    _constructorType :: Type,
    _constructorInductive :: Symbol
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Maybe Name,
    _inductiveKind :: Type,
    _inductiveConstructors :: [ConstructorInfo]
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''InductiveInfo

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoFunctions = mempty,
      _infoConstrs = mempty,
      _infoInductives = mempty
    }
