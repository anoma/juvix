module Juvix.Compiler.Core.Data.InfoTable
  ( module Juvix.Compiler.Core.Data.InfoTable,
    module Juvix.Compiler.Concrete.Data.Builtins,
  )
where

import Juvix.Compiler.Concrete.Data.Builtins
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
    _infoAxioms :: HashMap Text AxiomInfo,
    _infoIntToNat :: Maybe Symbol,
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
      _infoIntToNat = Nothing,
      _infoNextSymbol = 1,
      _infoNextTag = 0
    }

data IdentKind
  = IdentFun Symbol
  | IdentInd Symbol
  | IdentConstr Tag

data IdentifierInfo = IdentifierInfo
  { _identifierName :: Text,
    _identifierLocation :: Maybe Location,
    _identifierSymbol :: Symbol,
    _identifierType :: Type,
    _identifierArgsNum :: Int,
    _identifierArgsInfo :: [ArgumentInfo],
    _identifierIsExported :: Bool,
    _identifierBuiltin :: Maybe BuiltinFunction
  }

data ArgumentInfo = ArgumentInfo
  { _argumentName :: Text,
    _argumentLocation :: Maybe Location,
    _argumentType :: Type,
    _argumentIsImplicit :: IsImplicit
  }

data InductiveInfo = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [ConstructorInfo],
    _inductiveParams :: [ParameterInfo],
    _inductivePositive :: Bool,
    _inductiveBuiltin :: Maybe BuiltinInductive
  }

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    _constructorType :: Type,
    _constructorArgsNum :: Int,
    _constructorInductive :: Symbol,
    _constructorBuiltin :: Maybe BuiltinConstructor
  }

data ParameterInfo = ParameterInfo
  { _paramName :: Text,
    _paramLocation :: Maybe Location,
    _paramKind :: Type,
    _paramIsImplicit :: Bool
  }

data AxiomInfo = AxiomInfo
  { _axiomName :: Text,
    _axiomLocation :: Maybe Location,
    _axiomType :: Type
  }

makeLenses ''InfoTable
makeLenses ''IdentifierInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo
