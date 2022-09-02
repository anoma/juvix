module Juvix.Compiler.Core.Data.InfoTable where

import Juvix.Compiler.Core.Language

type IdentContext = HashMap Symbol Node

data InfoTable = InfoTable
  { _identContext :: IdentContext,
    -- `_identMap` is needed only for REPL
    _identMap :: HashMap Text IdentKind,
    _infoMain :: Maybe Symbol,
    _infoIdentifiers :: HashMap Symbol IdentifierInfo,
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
      _infoIdentifiers = mempty,
      _infoInductives = mempty,
      _infoConstructors = mempty,
      _infoAxioms = mempty
    }

data IdentKind = IdentSym Symbol | IdentTag Tag | IdentForward ForwardInfo

data ForwardInfo = ForwardInfo
  { _forwardName :: Text,
    _forwardSymbol :: Symbol,
    _forwardOffset :: Int
  }

data IdentifierInfo = IdentifierInfo
  { _identifierName :: Name,
    _identifierSymbol :: Symbol,
    _identifierType :: Type,
    -- _identifierArgsNum will be used often enough to justify avoiding recomputation
    _identifierArgsNum :: Int,
    _identifierArgsInfo :: [ArgumentInfo],
    _identifierIsExported :: Bool
  }

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Maybe Name,
    _argumentType :: Maybe Type,
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
makeLenses ''ForwardInfo
makeLenses ''IdentifierInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo
