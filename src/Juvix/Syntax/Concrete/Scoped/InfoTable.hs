module Juvix.Syntax.Concrete.Scoped.InfoTable where

import Juvix.Prelude
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Scoped.Name qualified as S

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

data CompileInfo = CompileInfo
  { _compileInfoBackendItems :: [BackendItem],
    _compileInfoDefined :: Interval
  }
  deriving stock (Eq, Show)

data InfoTable = InfoTable
  { _infoConstructors :: HashMap ConstructorRef ConstructorInfo,
    _infoAxioms :: HashMap AxiomRef AxiomInfo,
    _infoInductives :: HashMap InductiveRef InductiveInfo,
    _infoFunctions :: HashMap FunctionRef FunctionInfo,
    _infoFunctionClauses :: HashMap S.Symbol (FunctionClause 'Scoped),
    _infoNames :: [S.AName],
    _infoCompilationRules :: HashMap S.Symbol CompileInfo
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty,
      _infoFunctionClauses = mempty,
      _infoNames = mempty,
      _infoCompilationRules = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
makeLenses ''CompileInfo
