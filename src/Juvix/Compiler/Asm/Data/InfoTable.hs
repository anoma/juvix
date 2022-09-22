module Juvix.Compiler.Asm.Data.InfoTable where

import Juvix.Compiler.Asm.Language
import Juvix.Compiler.Asm.Language.Type

data InfoTable = InfoTable
  { _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoConstrs :: HashMap Tag ConstructorInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoMainFunction :: Maybe Symbol
  }

data FunctionInfo = FunctionInfo
  { _functionName :: Text,
    _functionLocation :: Maybe Location,
    _functionSymbol :: Symbol,
    -- | `_functionArgsNum` is the number of arguments the function takes at
    -- once. It should always be `<= length (typeArgs (_functionType))`.
    _functionArgsNum :: Int,
    _functionType :: Type,
    _functionCode :: Code
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    -- | `_constructorArgsNum` should always be equal to `length (typeArgs
    -- (_constructorType))`. It is stored separately mainly for the benefit of
    -- the interpreter (so it does not have to recompute it every time).
    _constructorArgsNum :: Int,
    _constructorType :: Type,
    _constructorInductive :: Symbol
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
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
      _infoInductives = mempty,
      _infoMainFunction = Nothing
    }
