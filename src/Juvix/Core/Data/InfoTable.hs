module Juvix.Core.Data.InfoTable where

import Juvix.Core.Language
import Juvix.Core.Language.Type

type IdentContext = HashMap Symbol Node

data InfoTable = InfoTable
  { _identContext :: IdentContext,
    _identInfo :: HashMap Symbol IdentInfo,
    _inductiveInfo :: HashMap Name InductiveInfo,
    _constructorInfo :: HashMap Tag ConstructorInfo,
    _axiomInfo :: HashMap Name AxiomInfo
  }

data IdentInfo = IdentInfo
  { _identName :: Name,
    _identSymbol :: Symbol,
    _identType :: Type,
    _identArgsNum :: Int,
    -- _identArgsNum will be used often enough to justify avoiding recomputation
    _identArgsInfo :: [ArgumentInfo],
    _identIsExported :: Bool
  }

data ArgumentInfo = ArgumentInfo
  { _argName :: Name,
    _argType :: Type,
    _argIsImplicit :: Bool
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
    _constructorType :: Type
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
