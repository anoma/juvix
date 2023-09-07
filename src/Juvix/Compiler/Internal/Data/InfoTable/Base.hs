module Juvix.Compiler.Internal.Data.InfoTable.Base where

import Juvix.Compiler.Internal.Data.InstanceInfo
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

data ConstructorInfo = ConstructorInfo
  { _constructorInfoInductiveParameters :: [InductiveParameter],
    _constructorInfoType :: Expression,
    _constructorInfoInductive :: InductiveName,
    _constructorInfoName :: ConstructorName,
    _constructorInfoBuiltin :: Maybe BuiltinConstructor,
    _constructorInfoTrait :: Bool
  }

newtype FunctionInfo = FunctionInfo
  { _functionInfoDef :: FunctionDef
  }

newtype AxiomInfo = AxiomInfo
  { _axiomInfoDef :: AxiomDef
  }

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef
  }

data InfoTable = InfoTable
  { _infoConstructors :: HashMap Name ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo,
    _infoFunctions :: HashMap Name FunctionInfo,
    _infoInductives :: HashMap Name InductiveInfo,
    _infoInstances :: InstanceTable
  }

makeLenses ''InfoTable
makeLenses ''FunctionInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''InductiveInfo

instance Semigroup InfoTable where
  a <> b =
    InfoTable
      { _infoConstructors = a ^. infoConstructors <> b ^. infoConstructors,
        _infoAxioms = a ^. infoAxioms <> b ^. infoAxioms,
        _infoFunctions = a ^. infoFunctions <> b ^. infoFunctions,
        _infoInductives = a ^. infoInductives <> b ^. infoInductives,
        _infoInstances = a ^. infoInstances <> b ^. infoInstances
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoConstructors = mempty,
        _infoAxioms = mempty,
        _infoFunctions = mempty,
        _infoInductives = mempty,
        _infoInstances = mempty
      }
