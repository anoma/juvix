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

instance Semigroup (InfoTable' n) where
  t1 <> t2 =
    InfoTable
      { _identContext = t1 ^. identContext <> t2 ^. identContext,
        _identMap = t1 ^. identMap <> t2 ^. identMap,
        _infoMain = (t1 ^. infoMain) <|> (t2 ^. infoMain),
        _infoIdentifiers = t1 ^. infoIdentifiers <> t2 ^. infoIdentifiers,
        _infoInductives = t1 ^. infoInductives <> t2 ^. infoInductives,
        _infoConstructors = t1 ^. infoConstructors <> t2 ^. infoConstructors,
        _infoAxioms = t1 ^. infoAxioms <> t2 ^. infoAxioms,
        _infoSpecialisations = t1 ^. infoSpecialisations <> t2 ^. infoSpecialisations,
        _infoLiteralIntToNat = (t1 ^. infoLiteralIntToNat) <|> (t2 ^. infoLiteralIntToNat),
        _infoLiteralIntToInt = (t1 ^. infoLiteralIntToInt) <|> (t2 ^. infoLiteralIntToInt),
        _infoBuiltins = t1 ^. infoBuiltins <> t2 ^. infoBuiltins
      }

instance Monoid (InfoTable' n) where
  mempty =
    InfoTable
      { _identContext = mempty,
        _identMap = mempty,
        _infoMain = Nothing,
        _infoIdentifiers = mempty,
        _infoInductives = mempty,
        _infoConstructors = mempty,
        _infoAxioms = mempty,
        _infoSpecialisations = mempty,
        _infoLiteralIntToNat = Nothing,
        _infoLiteralIntToInt = Nothing,
        _infoBuiltins = mempty
      }
