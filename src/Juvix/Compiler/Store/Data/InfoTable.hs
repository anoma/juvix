module Juvix.Compiler.Store.Data.InfoTable where

import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Internal.Data.Name
import Juvix.Compiler.Internal.Language as Internal
import Juvix.Compiler.Store.Language
import Juvix.Extra.Serialize

data InfoTable = InfoTable
  { _identMap :: Map Text IdentKind,
    _infoMain :: Maybe Symbol,
    _infoIdentifiers :: Map Symbol IdentifierInfo,
    _infoInductives :: Map Symbol InductiveInfo,
    _infoConstructors :: Map Tag ConstructorInfo,
    _infoAxioms :: Map Text AxiomInfo,
    _infoSpecialisations :: Map Symbol [SpecialisationInfo],
    _infoBuiltins :: Map BuiltinPrim IdentKind,
    _infoPrecedenceGraph :: Map NameId (Set NameId)
  }
  deriving stock (Generic)

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _identMap = mempty,
      _infoMain = Nothing,
      _infoIdentifiers = mempty,
      _infoInductives = mempty,
      _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoSpecialisations = mempty,
      _infoBuiltins = mempty,
      _infoPrecedenceGraph = mempty
    }

data IdentKind
  = IdentFun Symbol
  | IdentInd Symbol
  | IdentConstr Tag
  deriving stock (Generic)

data IdentifierInfo = IdentifierInfo
  { _identifierName :: Name,
    _identifierLocation :: Maybe Location,
    _identifierSymbol :: Symbol,
    _identifierInternalType :: Internal.Expression,
    _identifierInternalDef :: Maybe Internal.Expression,
    _identifierType :: Type,
    -- | The number of lambdas in the identifier body
    _identifierArgsNum :: Int,
    _identifierIsExported :: Bool,
    _identifierIsTerminating :: Bool,
    _identifierIsInstance :: Bool,
    _identifierIsCoercion :: Bool,
    _identifierBuiltin :: Maybe BuiltinFunction,
    _identifierPragmas :: Pragmas,
    _identifierArgNames :: [Maybe Text],
    _identifierNode :: Node
  }
  deriving stock (Generic)

data InductiveInfo = InductiveInfo
  { _inductiveName :: Name,
    _inductiveLocation :: Maybe Location,
    _inductiveSymbol :: Symbol,
    _inductiveKind :: Type,
    _inductiveConstructors :: [Tag],
    _inductiveParams :: [ParameterInfo],
    _inductivePositive :: Bool,
    _inductiveBuiltin :: Maybe BuiltinType,
    _inductivePragmas :: Pragmas,
    _inductiveIsTrait :: Bool
  }
  deriving stock (Generic)

data ConstructorInfo = ConstructorInfo
  { _constructorName :: Name,
    _constructorLocation :: Maybe Location,
    _constructorTag :: Tag,
    _constructorInternalType :: Internal.Expression,
    _constructorType :: Type,
    _constructorArgsNum :: Int,
    _constructorInductive :: Symbol,
    _constructorFixity :: Maybe Fixity,
    _constructorBuiltin :: Maybe BuiltinConstructor,
    _constructorPragmas :: Pragmas
  }
  deriving stock (Generic)

data ParameterInfo = ParameterInfo
  { _paramName :: Name,
    _paramLocation :: Maybe Location,
    _paramInternalKind :: Internal.Expression,
    _paramKind :: Type,
    _paramIsImplicit :: Bool
  }
  deriving stock (Generic)

data AxiomInfo = AxiomInfo
  { _axiomName :: Name,
    _axiomLocation :: Maybe Location,
    _axiomType :: Type,
    _axiomPragmas :: Pragmas
  }
  deriving stock (Generic)

data SpecialisationInfo = SpecialisationInfo
  { _specSignature :: ([Node], [Int]),
    _specSymbol :: Symbol
  }
  deriving stock (Generic)

instance Serialize InfoTable

instance Serialize IdentKind

instance Serialize IdentifierInfo

instance Serialize InductiveInfo

instance Serialize ConstructorInfo

instance Serialize ParameterInfo

instance Serialize AxiomInfo

instance Serialize SpecialisationInfo

makeLenses ''InfoTable
makeLenses ''IdentifierInfo
makeLenses ''ConstructorInfo
makeLenses ''InductiveInfo
makeLenses ''ParameterInfo
makeLenses ''AxiomInfo
makeLenses ''SpecialisationInfo
