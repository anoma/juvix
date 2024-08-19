module Juvix.Compiler.Store.Internal.Data.InfoTable where

import Juvix.Compiler.Internal.Language
import Juvix.Extra.Serialize
import Juvix.Prelude

data ConstructorInfo = ConstructorInfo
  { _constructorInfoInductiveParameters :: [InductiveParameter],
    _constructorInfoType :: Expression,
    _constructorInfoInductive :: InductiveName,
    _constructorInfoName :: ConstructorName,
    _constructorInfoBuiltin :: Maybe BuiltinConstructor,
    _constructorInfoTrait :: Bool,
    _constructorInfoRecord :: Bool
  }
  deriving stock (Generic)

instance Serialize ConstructorInfo

instance NFData ConstructorInfo

data FunctionInfo = FunctionInfo
  { _functionInfoName :: FunctionName,
    _functionInfoType :: Expression,
    _functionInfoTerminating :: Bool,
    _functionInfoInstance :: Bool,
    _functionInfoCoercion :: Bool,
    _functionInfoBuiltin :: Maybe BuiltinFunction,
    _functionInfoArgsInfo :: [ArgInfo],
    _functionInfoPragmas :: Pragmas,
    _functionInfoIsLocal :: Bool
  }
  deriving stock (Generic)

instance Serialize FunctionInfo

instance NFData FunctionInfo

newtype AxiomInfo = AxiomInfo
  { _axiomInfoDef :: AxiomDef
  }
  deriving stock (Generic)

instance Serialize AxiomInfo

instance NFData AxiomInfo

data InductiveInfo = InductiveInfo
  { _inductiveInfoName :: InductiveName,
    _inductiveInfoBuiltin :: Maybe BuiltinInductive,
    _inductiveInfoType :: Expression,
    _inductiveInfoParameters :: [InductiveParameter],
    _inductiveInfoConstructors :: [ConstrName],
    _inductiveInfoPositive :: Bool,
    _inductiveInfoTrait :: Bool,
    _inductiveInfoPragmas :: Pragmas
  }
  deriving stock (Generic)

instance Serialize InductiveInfo

instance NFData InductiveInfo

data InfoTable = InfoTable
  { _infoConstructors :: HashMap Name ConstructorInfo,
    _infoAxioms :: HashMap Name AxiomInfo,
    _infoFunctions :: HashMap Name FunctionInfo,
    _infoInductives :: HashMap Name InductiveInfo
  }
  deriving stock (Generic)

instance Serialize InfoTable

instance NFData InfoTable

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
        _infoInductives = a ^. infoInductives <> b ^. infoInductives
      }

instance Monoid InfoTable where
  mempty =
    InfoTable
      { _infoConstructors = mempty,
        _infoAxioms = mempty,
        _infoFunctions = mempty,
        _infoInductives = mempty
      }
