module Juvix.Core.Context where

import Juvix.Core.Node
import Juvix.Core.Prelude
import Juvix.Core.Type

type IdentContext = HashMap Symbol Node

data Context = Context
  { _identContext :: IdentContext,
    _identInfo :: HashMap Symbol IdentInfo,
    -- We reuse `Name` for runtime-irrelevant identifiers (inductive type names,
    -- axiom names, etc). We shouldn't do this for Symbol and Tag, because we
    -- need them "small", consecutive and separate for the code generator.
    -- Discuss: advantages/disadvantages of doing this separation later in the
    -- pipeline.
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
    _identArgsInfo :: [ArgumentInfo]
  }

data ArgumentInfo = ArgumentInfo
  { _argName :: Name,
    _argType :: Type,
    _argIsImplicit :: Bool
    -- future: _argIsLazy :: Bool
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

makeLenses ''Context
makeLenses ''IdentInfo
makeLenses ''ArgumentInfo
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo
