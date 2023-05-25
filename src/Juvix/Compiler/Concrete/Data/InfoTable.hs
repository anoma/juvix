module Juvix.Compiler.Concrete.Data.InfoTable where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data FunctionInfo = FunctionInfo
  { _functionInfoType :: TypeSignature 'Scoped,
    _functionInfoClauses :: [FunctionClause 'Scoped]
  }
  deriving stock (Eq, Show)

newtype ConstructorInfo = ConstructorInfo
  { _constructorDef :: InductiveConstructorDef 'Scoped
  }
  deriving stock (Eq, Show)

newtype AxiomInfo = AxiomInfo
  { _axiomDef :: AxiomDef 'Scoped
  }
  deriving stock (Eq, Show)

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef 'Scoped
  }
  deriving stock (Eq, Show)

type DocTable = HashMap NameId (Judoc 'Scoped)

data InfoTable = InfoTable
  { _infoConstructors :: HashMap S.NameId ConstructorInfo,
    _infoModules :: HashMap S.TopModulePath (Module 'Scoped 'ModuleTop),
    _infoAxioms :: HashMap S.NameId AxiomInfo,
    _infoInductives :: HashMap S.NameId InductiveInfo,
    _infoFunctions :: HashMap S.NameId FunctionInfo
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoModules = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
