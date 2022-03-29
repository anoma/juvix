module MiniJuvix.Syntax.Abstract.InfoTable where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language

newtype FunctionInfo = FunctionInfo {
  _functionInfoDef :: FunctionDef
  }

data ConstructorInfo = ConstructorInfo {
  _constructorInfoInductive :: InductiveInfo,
  _constructorInfoType :: Expression
  }

data AxiomInfo = AxiomInfo {
  _axiomInfoType :: Expression,
  _axiomInfoBackends :: [BackendItem]
  }

newtype InductiveInfo = InductiveInfo {
  _inductiveInfoDef :: InductiveDef
  }

data InfoTable = InfoTable {
  _infoConstructors :: HashMap ConstructorRef ConstructorInfo,
  _infoAxioms :: HashMap AxiomRef AxiomInfo,
  _infoInductives :: HashMap InductiveRef InductiveInfo,
  _infoFunctions :: HashMap FunctionRef FunctionInfo
  }

emptyInfoTable :: InfoTable
emptyInfoTable = InfoTable {
  _infoConstructors = mempty,
  _infoAxioms = mempty,
  _infoInductives = mempty,
  _infoFunctions = mempty
  }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
