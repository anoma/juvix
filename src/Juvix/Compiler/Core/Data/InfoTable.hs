module Juvix.Compiler.Core.Data.InfoTable where

import Juvix.Compiler.Core.Language

type IdentContext = HashMap Symbol Node

data InfoTable = InfoTable
  { _identContext :: IdentContext,
    -- `_identMap` is needed only for REPL
    _identMap :: HashMap Text IdentKind,
    _infoMain :: Maybe Symbol,
    _infoIdentifiers :: HashMap Symbol IdentifierInfo,
    _infoInductives :: HashMap Symbol InductiveInfo,
    _infoConstructors :: HashMap Tag ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo,
    _infoNextSymbol :: Word,
    _infoNextTag :: Word
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _identContext = mempty,
      _identMap = mempty,
      _infoMain = Nothing,
      _infoIdentifiers = mempty,
      _infoInductives = mempty,
      _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoNextSymbol = 0,
      _infoNextTag = 0
    }

data IdentKind
  = IdentFun Symbol
  | IdentInd Symbol
  | IdentConstr Tag

data IdentifierInfo = IdentifierInfo
  { _identifierName :: Maybe Name,
    _identifierSymbol :: Symbol,
    _identifierType :: Type,
    -- _identifierArgsNum will be used often enough to justify avoiding recomputation
    _identifierArgsNum :: Int,
    _identifierArgsInfo :: [ArgumentInfo],
    _identifierIsExported :: Bool
  }

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Maybe Name,
    _argumentType :: Type,
    _argumentIsImplicit :: IsImplicit
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Name,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [ConstructorInfo],
    _inductiveParams :: [ParameterInfo],
    _inductivePositive :: Bool
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Name,
    _constructorTag :: Tag,
    _constructorType :: Type,
    _constructorArgsNum :: Int,
    _constructorInductive :: Symbol
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
makeLenses ''IdentifierInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo
