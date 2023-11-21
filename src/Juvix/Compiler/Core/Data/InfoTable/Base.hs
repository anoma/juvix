module Juvix.Compiler.Core.Data.InfoTable.Base where

import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Core.Language.Base
import Juvix.Extra.Serialize

data InfoTable' n = InfoTable
  { _identContext :: HashMap Symbol n,
    _identMap :: HashMap Text IdentKind,
    _infoMain :: Maybe Symbol,
    _infoIdentifiers :: HashMap Symbol (IdentifierInfo' n),
    _infoInductives :: HashMap Symbol (InductiveInfo' n),
    _infoConstructors :: HashMap Tag (ConstructorInfo' n),
    _infoAxioms :: HashMap Text (AxiomInfo' n),
    _infoSpecialisations :: HashMap Symbol [SpecialisationInfo' n],
    _infoLiteralIntToNat :: Maybe Symbol,
    _infoLiteralIntToInt :: Maybe Symbol,
    _infoBuiltins :: HashMap BuiltinPrim IdentKind
  }
  deriving stock (Generic)

data IdentKind
  = IdentFun Symbol
  | IdentInd Symbol
  | IdentConstr Tag
  deriving stock (Generic)

data IdentifierInfo' n = IdentifierInfo
  { _identifierName :: Text,
    _identifierLocation :: Maybe Location,
    _identifierSymbol :: Symbol,
    _identifierType :: n,
    -- | The number of lambdas in the identifier body
    _identifierArgsNum :: Int,
    _identifierIsExported :: Bool,
    _identifierBuiltin :: Maybe BuiltinFunction,
    _identifierPragmas :: Pragmas,
    _identifierArgNames :: [Maybe Text]
  }
  deriving stock (Generic)

data InductiveInfo' n = InductiveInfo
  { _inductiveName :: Text,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: n,
    _inductiveConstructors :: [Tag],
    _inductiveParams :: [ParameterInfo' n],
    _inductivePositive :: Bool,
    _inductiveBuiltin :: Maybe BuiltinType,
    _inductivePragmas :: Pragmas
  }
  deriving stock (Generic)

data ConstructorInfo' n = ConstructorInfo
  { _constructorName :: Text,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    _constructorType :: n,
    _constructorArgsNum :: Int,
    _constructorInductive :: Symbol,
    _constructorFixity :: Maybe Fixity,
    _constructorBuiltin :: Maybe BuiltinConstructor,
    _constructorPragmas :: Pragmas
  }
  deriving stock (Generic)

data ParameterInfo' n = ParameterInfo
  { _paramName :: Text,
    _paramLocation :: Maybe Location,
    _paramKind :: n,
    _paramIsImplicit :: Bool
  }
  deriving stock (Generic)

data AxiomInfo' n = AxiomInfo
  { _axiomName :: Text,
    _axiomLocation :: Maybe Location,
    _axiomType :: n,
    _axiomPragmas :: Pragmas
  }
  deriving stock (Generic)

data SpecialisationInfo' n = SpecialisationInfo
  { _specSignature :: ([n], [Int]),
    _specSymbol :: Symbol
  }
  deriving stock (Generic)

instance (Serialize n) => Serialize (InfoTable' n)

instance Serialize IdentKind

instance (Serialize n) => Serialize (IdentifierInfo' n)

instance (Serialize n) => Serialize (InductiveInfo' n)

instance (Serialize n) => Serialize (ConstructorInfo' n)

instance (Serialize n) => Serialize (ParameterInfo' n)

instance (Serialize n) => Serialize (AxiomInfo' n)

instance (Serialize n) => Serialize (SpecialisationInfo' n)

makeLenses ''InfoTable'
makeLenses ''IdentifierInfo'
makeLenses ''InductiveInfo'
makeLenses ''ConstructorInfo'
makeLenses ''ParameterInfo'
makeLenses ''AxiomInfo'
makeLenses ''SpecialisationInfo'
