module Juvix.Compiler.Core.Data.InfoTable where

import Juvix.Compiler.Core.Language

type IdentContext = HashMap Symbol Node

data InfoTable = InfoTable
  { _identContext :: IdentContext,
    -- `_identMap` is needed only for REPL
    _identMap :: HashMap Text (Either Symbol Tag),
    _infoMain :: Maybe Symbol,
    _infoIdents :: HashMap Symbol IdentInfo,
    _infoInductives :: HashMap Name InductiveInfo,
    _infoConstructors :: HashMap Tag ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _identContext = mempty,
      _identMap = mempty,
      _infoMain = Nothing,
      _infoIdents = mempty,
      _infoInductives = mempty,
      _infoConstructors = mempty,
      _infoAxioms = mempty
    }

data IdentInfo = IdentInfo
  { _identName :: Name,
    _identSymbol :: Symbol,
    _identType :: Type,
    -- _identArgsNum will be used often enough to justify avoiding recomputation
    _identArgsNum :: Int,
    _identArgsInfo :: [ArgumentInfo],
    _identIsExported :: Bool
  }

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Name,
    _argumentType :: Type,
    _argumentIsImplicit :: Bool
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Name,
    _inductiveKind :: Type,
    _inductiveConstructors :: [ConstructorInfo],
    _inductiveParams :: [ParameterInfo],
    _inductivePositive :: Bool
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Name,
    _constructorTag :: Tag,
    _constructorType :: Type,
    _constructorArgsNum :: Int
  }

data ParameterInfo = ParameterInfo
  { _paramName :: Name,
    _paramKind :: Type,
    _paramIsImplicit :: Bool
  }

data AxiomInfo = AxiomInfo
  { _axiomName :: Name,
    _axiomType :: Type
  }

makeLenses ''InfoTable
makeLenses ''IdentInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo
