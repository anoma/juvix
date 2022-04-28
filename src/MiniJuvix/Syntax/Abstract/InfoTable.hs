module MiniJuvix.Syntax.Abstract.InfoTable where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S

newtype FunctionInfo = FunctionInfo
  { _functionInfoDef :: FunctionDef
  }

data ConstructorInfo = ConstructorInfo
  { _constructorInfoInductive :: InductiveInfo,
    _constructorInfoType :: Expression
  }

newtype AxiomInfo = AxiomInfo
  { _axiomInfoType :: Expression
  }

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef
  }

data InfoTable = InfoTable
  { _infoConstructors :: HashMap ConstructorRef ConstructorInfo,
    _infoAxioms :: HashMap AxiomRef AxiomInfo,
    _infoInductives :: HashMap InductiveRef InductiveInfo,
    _infoFunctions :: HashMap FunctionRef FunctionInfo,
    _infoCompilationRules :: HashMap S.Symbol [BackendItem]
  }

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty,
      _infoCompilationRules = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
