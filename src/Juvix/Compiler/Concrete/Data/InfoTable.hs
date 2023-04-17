module Juvix.Compiler.Concrete.Data.InfoTable where

import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

newtype FunctionInfo = FunctionInfo
  { _functionInfoType :: Expression
  }
  deriving stock (Eq, Show)

newtype ConstructorInfo = ConstructorInfo
  { _constructorInfoType :: Expression
  }
  deriving stock (Eq, Show)

newtype AxiomInfo = AxiomInfo
  { _axiomInfoType :: Expression
  }
  deriving stock (Eq, Show)

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef 'Scoped
  }
  deriving stock (Eq, Show)

data InfoTable = InfoTable
  { _infoConstructors :: HashMap ConstructorRef ConstructorInfo,
    _infoModules :: HashMap S.TopModulePath (Module 'Scoped 'ModuleTop),
    _infoAxioms :: HashMap AxiomRef AxiomInfo,
    _infoInductives :: HashMap InductiveRef InductiveInfo,
    _infoFunctions :: HashMap FunctionRef FunctionInfo,
    _infoDoc :: HashMap NameId (Judoc 'Scoped),
    _infoFunctionClauses :: HashMap S.Symbol (FunctionClause 'Scoped),
    _infoNames :: [S.AName]
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoModules = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty,
      _infoDoc = mempty,
      _infoFunctionClauses = mempty,
      _infoNames = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
