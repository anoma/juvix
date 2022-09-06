module Juvix.Compiler.Core.Data.Stripped.InfoTable where

import Juvix.Compiler.Core.Language.Stripped

data InfoTable = InfoTable
  { _infoMain :: Maybe Symbol,
    _infoFunctions :: HashMap Symbol FunctionInfo,
    _infoInductives :: HashMap Name InductiveInfo,
    _infoConstructors :: HashMap Tag ConstructorInfo
  }

data FunctionInfo = FunctionInfo
  { _functionName :: Maybe Name,
    _functionSymbol :: Symbol,
    -- _functionNode has `_functionArgsNum` free variables corresponding to the
    -- function arguments
    _functionNode :: Node,
    _functionType :: Type,
    -- a function can have 0 arguments
    _functionArgsNum :: Int,
    _functionArgsInfo :: [ArgumentInfo],
    _functionIsExported :: Bool
  }

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Maybe Name,
    _argumentType :: Type
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Name,
    _inductiveKind :: Type,
    _inductiveConstructors :: [ConstructorInfo],
    _inductiveParams :: [ParameterInfo],
    _inductivePositive :: Bool
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Maybe Name,
    _constructorTag :: Tag,
    _constructorType :: Type
  }

data ParameterInfo = ParameterInfo
  { _paramName :: Maybe Name,
    _paramKind :: Type,
    _paramIsImplicit :: Bool
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
