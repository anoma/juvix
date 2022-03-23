{-# LANGUAGE UndecidableInstances #-}

module MiniJuvix.Syntax.Concrete.Language
  ( module MiniJuvix.Syntax.Concrete.Language,
    module MiniJuvix.Syntax.Concrete.Name,
    module MiniJuvix.Syntax.Concrete.Loc,
    module MiniJuvix.Syntax.Concrete.PublicAnn,
    module MiniJuvix.Syntax.Concrete.ModuleIsTop,
    module MiniJuvix.Syntax.Concrete.Language.Stage,
    module MiniJuvix.Syntax.Fixity,
    module MiniJuvix.Syntax.Usage,
    module MiniJuvix.Syntax.Universe,
  )
where

--------------------------------------------------------------------------------

import qualified Data.Kind as GHC
import MiniJuvix.Prelude hiding (show)
import MiniJuvix.Syntax.Concrete.Language.Stage
import MiniJuvix.Syntax.Concrete.Loc
import MiniJuvix.Syntax.Concrete.ModuleIsTop
import MiniJuvix.Syntax.Concrete.Name
import MiniJuvix.Syntax.Concrete.PublicAnn
import MiniJuvix.Syntax.Concrete.Scoped.Name (unqualifiedSymbol)
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Fixity
import MiniJuvix.Syntax.Universe
import MiniJuvix.Syntax.Usage
import Prelude (show)

--------------------------------------------------------------------------------
-- Parsing stages
--------------------------------------------------------------------------------
type family RefNameType (c :: S.IsConcrete) :: (res :: GHC.Type) | res -> c where
  RefNameType 'S.Concrete = S.Name
  RefNameType 'S.NotConcrete = S.Name' ()

type family SymbolType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  SymbolType 'Parsed = Symbol
  SymbolType 'Scoped = S.Symbol

type family ModuleRefType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  ModuleRefType 'Parsed = Name
  ModuleRefType 'Scoped = ModuleRef

type family IdentifierType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  IdentifierType 'Parsed = Name
  IdentifierType 'Scoped = ScopedIden

type family PatternAtomIdenType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  PatternAtomIdenType 'Parsed = Name
  PatternAtomIdenType 'Scoped = PatternScopedIden

type family ExpressionType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  ExpressionType 'Parsed = ExpressionAtoms 'Parsed
  ExpressionType 'Scoped = Expression

type family PatternType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  PatternType 'Parsed = PatternAtom 'Parsed
  PatternType 'Scoped = Pattern

type family ImportType (s :: Stage) :: GHC.Type where
  ImportType 'Parsed = TopModulePath
  ImportType 'Scoped = Module 'Scoped 'ModuleTop

type family
  ModulePathType (s :: Stage) (t :: ModuleIsTop) ::
    (res :: GHC.Type) | res -> t s
  where
  ModulePathType 'Parsed 'ModuleTop = TopModulePath
  ModulePathType 'Scoped 'ModuleTop = S.TopModulePath
  ModulePathType 'Parsed 'ModuleLocal = Symbol
  ModulePathType 'Scoped 'ModuleLocal = S.Symbol

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

data Statement (s :: Stage)
  = StatementOperator OperatorSyntaxDef
  | StatementTypeSignature (TypeSignature s)
  | StatementImport (Import s)
  | StatementInductive (InductiveDef s)
  | StatementModule (Module s 'ModuleLocal)
  | StatementOpenModule (OpenModule s)
  | StatementFunctionClause (FunctionClause s)
  | StatementAxiom (AxiomDef s)
  | StatementEval (Eval s)
  | StatementPrint (Print s)
  | StatementForeign ForeignBlock

deriving stock instance
  ( Show (ImportType s),
    Show (ModulePathType s 'ModuleLocal),
    Show (PatternType s),
    Show (SymbolType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (ExpressionType s)
  ) =>
  Show (Statement s)

deriving stock instance
  ( Eq (ImportType s),
    Eq (PatternType s),
    Eq (ModulePathType s 'ModuleLocal),
    Eq (SymbolType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Statement s)

deriving stock instance
  ( Ord (ImportType s),
    Ord (PatternType s),
    Ord (ModulePathType s 'ModuleLocal),
    Ord (SymbolType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Statement s)

data ForeignBlock = ForeignBlock
  { _foreignBackend :: Backend,
    _foreignCode :: Text
  }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

newtype Import (s :: Stage) = Import
  { importModule :: ImportType s
  }

deriving stock instance (Show (ImportType s)) => Show (Import s)

deriving stock instance (Eq (ImportType s)) => Eq (Import s)

deriving stock instance (Ord (ImportType s)) => Ord (Import s)

instance HasLoc (Import 'Parsed) where
  getLoc (Import t) = getLoc t

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

data OperatorSyntaxDef = OperatorSyntaxDef
  { opSymbol :: Symbol,
    opFixity :: Fixity
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc OperatorSyntaxDef where
  getLoc = getLoc . opSymbol

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature (s :: Stage) = TypeSignature
  { _sigName :: FunctionName s,
    _sigType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (TypeSignature s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (TypeSignature s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (TypeSignature s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef (s :: Stage) = AxiomDef
  { _axiomName :: SymbolType s,
    _axiomType :: ExpressionType s,
    _axiomBackendItems :: [BackendItem]
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (AxiomDef s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (AxiomDef s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (AxiomDef s)

-------------------------------------------------------------------------------
-- Lift type construction declaration
-------------------------------------------------------------------------------

type InductiveConstructorName s = SymbolType s

type InductiveName s = SymbolType s

data InductiveConstructorDef (s :: Stage) = InductiveConstructorDef
  { constructorName :: InductiveConstructorName s,
    constructorType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveConstructorDef s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveConstructorDef s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveConstructorDef s)

data InductiveParameter (s :: Stage) = InductiveParameter
  { _inductiveParameterName :: SymbolType s,
    _inductiveParameterType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveParameter s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveParameter s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveParameter s)

data InductiveDef (s :: Stage) = InductiveDef
  { _inductiveName :: InductiveName s,
    _inductiveParameters :: [InductiveParameter s],
    _inductiveType :: Maybe (ExpressionType s),
    _inductiveConstructors :: [InductiveConstructorDef s]
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveDef s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveDef s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveDef s)

--------------------------------------------------------------------------------
-- Pattern
--------------------------------------------------------------------------------

data PatternApp = PatternApp
  { patAppLeft :: Pattern,
    patAppRight :: Pattern
  }
  deriving stock (Show, Eq, Ord)

data PatternInfixApp = PatternInfixApp
  { patInfixLeft :: Pattern,
    patInfixConstructor :: ConstructorRef,
    patInfixRight :: Pattern
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PatternInfixApp where
  getFixity (PatternInfixApp _ op _) = fromMaybe impossible (_constructorRefName op ^. S.nameFixity)

data PatternPostfixApp = PatternPostfixApp
  { patPostfixParameter :: Pattern,
    patPostfixConstructor :: ConstructorRef
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PatternPostfixApp where
  getFixity (PatternPostfixApp _ op) = fromMaybe impossible (_constructorRefName op ^. S.nameFixity)

data Pattern
  = PatternVariable (SymbolType 'Scoped)
  | PatternConstructor ConstructorRef
  | PatternApplication PatternApp
  | PatternInfixApplication PatternInfixApp
  | PatternPostfixApplication PatternPostfixApp
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show, Eq, Ord)

instance HasAtomicity Pattern where
  atomicity e = case e of
    PatternVariable {} -> Atom
    PatternConstructor {} -> Atom
    PatternApplication {} -> Aggregate appFixity
    PatternInfixApplication a -> Aggregate (getFixity a)
    PatternPostfixApplication p -> Aggregate (getFixity p)
    PatternWildcard -> Atom
    PatternEmpty -> Atom

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternScopedIden
  = PatternScopedVar S.Symbol
  | PatternScopedConstructor ConstructorRef
  deriving stock (Show, Ord, Eq)

data PatternAtom (s :: Stage)
  = PatternAtomIden (PatternAtomIdenType s)
  | PatternAtomWildcard
  | PatternAtomEmpty
  | PatternAtomParens (PatternAtoms s)

instance HasAtomicity (PatternAtom 'Parsed) where
  atomicity = const Atom

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (PatternAtomIdenType s),
    Show (PatternType s)
  ) =>
  Show (PatternAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (PatternAtomIdenType s),
    Eq (PatternType s)
  ) =>
  Eq (PatternAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (PatternAtomIdenType s),
    Ord (PatternType s)
  ) =>
  Ord (PatternAtom s)

newtype PatternAtoms (s :: Stage)
  = PatternAtoms (NonEmpty (PatternAtom s))

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (PatternAtomIdenType s),
    Show (PatternType s)
  ) =>
  Show (PatternAtoms s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (PatternAtomIdenType s),
    Eq (PatternType s)
  ) =>
  Eq (PatternAtoms s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (PatternAtomIdenType s),
    Ord (PatternType s)
  ) =>
  Ord (PatternAtoms s)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type FunctionName s = SymbolType s

data FunctionClause (s :: Stage) = FunctionClause
  { _clauseOwnerFunction :: FunctionName s,
    _clausePatterns :: [PatternType s],
    _clauseBody :: ExpressionType s,
    _clauseWhere :: Maybe (WhereBlock s)
  }

deriving stock instance
  ( Show (PatternType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (FunctionClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (FunctionClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (FunctionClause s)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

type LocalModuleName s = SymbolType s

data Module (s :: Stage) (t :: ModuleIsTop) = Module
  { _modulePath :: ModulePathType s t,
    _moduleParameters :: [InductiveParameter s],
    _moduleBody :: [Statement s]
  }

deriving stock instance
  ( Show (ModulePathType s t),
    Show (ModulePathType s 'ModuleLocal),
    Show (ImportType s),
    Show (PatternType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (Module s t)

deriving stock instance
  ( Eq (ModulePathType s t),
    Eq (ModulePathType s 'ModuleLocal),
    Eq (ImportType s),
    Eq (PatternType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Module s t)

deriving stock instance
  ( Ord (ModulePathType s t),
    Ord (ModulePathType s 'ModuleLocal),
    Ord (ImportType s),
    Ord (PatternType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Module s t)

data UsingHiding
  = Using (NonEmpty Symbol)
  | Hiding (NonEmpty Symbol)
  deriving stock (Show, Eq, Ord)

type ModuleRef = ModuleRef' 'S.Concrete

newtype ModuleRef' (c :: S.IsConcrete) = ModuleRef'
  { _unModuleRef' :: Σ ModuleIsTop (TyCon1 (ModuleRef'' c))
  }

-- | TODO can this be derived?
instance SingI c => Show (ModuleRef' c) where
  show (ModuleRef' (isTop :&: r)) = case isTop of
    SModuleLocal -> case sing :: S.SIsConcrete c of
      S.SConcrete -> show r
      S.SNotConcrete -> show r
    SModuleTop -> case sing :: S.SIsConcrete c of
      S.SConcrete -> show r
      S.SNotConcrete -> show r

getNameRefId :: forall c. SingI c => RefNameType c -> S.NameId
getNameRefId = case sing :: S.SIsConcrete c of
  S.SConcrete -> S._nameId
  S.SNotConcrete -> S._nameId

getModuleExportInfo :: ModuleRef' c -> ExportInfo
getModuleExportInfo = projSigma2 _moduleExportInfo . _unModuleRef'

getModuleRefNameType :: ModuleRef' c -> RefNameType c
getModuleRefNameType = projSigma2 _moduleRefName . _unModuleRef'

instance SingI c => Eq (ModuleRef' c) where
  (==) = (==) `on` (getNameRefId . getModuleRefNameType)

instance SingI c => Ord (ModuleRef' c) where
  compare = compare `on` (getNameRefId . getModuleRefNameType)

-- TODO find a better name
data ModuleRef'' (c :: S.IsConcrete) (t :: ModuleIsTop) = ModuleRef''
  { _moduleRefName :: RefNameType c,
    _moduleExportInfo :: ExportInfo,
    _moduleRefModule :: Module 'Scoped t
  }

instance Show (RefNameType s) => Show (ModuleRef'' s t) where
  show = show . _moduleRefName

data SymbolEntry
  = EntryAxiom (AxiomRef' 'S.NotConcrete)
  | EntryInductive (InductiveRef' 'S.NotConcrete)
  | EntryFunction (FunctionRef' 'S.NotConcrete)
  | EntryConstructor (ConstructorRef' 'S.NotConcrete)
  | -- | TODO does this ever contain top modules?
    EntryModule (ModuleRef' 'S.NotConcrete)
  deriving stock (Show)

-- | Symbols that a module exports
newtype ExportInfo = ExportInfo
  { _exportSymbols :: HashMap Symbol SymbolEntry
  }
  deriving stock (Show)

data OpenModule (s :: Stage) = OpenModule
  { openModuleName :: ModuleRefType s,
    openParameters :: [ExpressionType s],
    openUsingHiding :: Maybe UsingHiding,
    openPublic :: PublicAnn
  }

deriving stock instance
  ( Eq (IdentifierType s),
    Eq (SymbolType s),
    Eq (ModuleRefType s),
    Eq (PatternType s),
    Eq (ExpressionType s)
  ) =>
  Eq (OpenModule s)

deriving stock instance
  ( Ord (IdentifierType s),
    Ord (SymbolType s),
    Ord (PatternType s),
    Ord (ModuleRefType s),
    Ord (ExpressionType s)
  ) =>
  Ord (OpenModule s)

deriving stock instance
  ( Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (ExpressionType s)
  ) =>
  Show (OpenModule s)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

type AxiomRef = AxiomRef' 'S.Concrete

data AxiomRef' (n :: S.IsConcrete) = AxiomRef'
  { _axiomRefName :: RefNameType n,
    _axiomRefType :: Expression,
    _axiomRefBackends :: [BackendItem]
  }

instance Eq (RefNameType s) => Eq (AxiomRef' s) where
  (==) = (==) `on` _axiomRefName

instance Ord (RefNameType s) => Ord (AxiomRef' s) where
  compare = compare `on` _axiomRefName

instance Show (RefNameType s) => Show (AxiomRef' s) where
  show = show . _axiomRefName

type InductiveRef = InductiveRef' 'S.Concrete

data InductiveRef' (n :: S.IsConcrete) = InductiveRef'
  { _inductiveRefName :: RefNameType n,
    _inductiveRefDef :: InductiveDef 'Scoped
  }

instance Eq (RefNameType s) => Eq (InductiveRef' s) where
  (==) = (==) `on` _inductiveRefName

instance Ord (RefNameType s) => Ord (InductiveRef' s) where
  compare = compare `on` _inductiveRefName

instance Show (RefNameType s) => Show (InductiveRef' s) where
  show = show . _inductiveRefName

type FunctionRef = FunctionRef' 'S.Concrete

data FunctionRef' (n :: S.IsConcrete) = FunctionRef'
  { _functionRefName :: RefNameType n,
    _functionRefSig :: Expression
  }

instance Eq (RefNameType s) => Eq (FunctionRef' s) where
  (==) = (==) `on` _functionRefName

instance Ord (RefNameType s) => Ord (FunctionRef' s) where
  compare = compare `on` _functionRefName

instance Show (RefNameType s) => Show (FunctionRef' s) where
  show = show . _functionRefName

type ConstructorRef = ConstructorRef' 'S.Concrete

data ConstructorRef' (n :: S.IsConcrete) = ConstructorRef'
  { _constructorRefName :: RefNameType n,
    _constructorSig :: Expression
  }

instance Eq (RefNameType s) => Eq (ConstructorRef' s) where
  (==) = (==) `on` _constructorRefName

instance Ord (RefNameType s) => Ord (ConstructorRef' s) where
  compare = compare `on` _constructorRefName

instance Show (RefNameType s) => Show (ConstructorRef' s) where
  show = show . _constructorRefName

type ScopedIden = ScopedIden' 'S.Concrete

data ScopedIden' (n :: S.IsConcrete)
  = ScopedAxiom (AxiomRef' n)
  | ScopedInductive (InductiveRef' n)
  | ScopedVar S.Symbol
  | ScopedFunction (FunctionRef' n)
  | ScopedConstructor (ConstructorRef' n)

deriving stock instance
  (Eq (RefNameType s)) => Eq (ScopedIden' s)

deriving stock instance
  (Ord (RefNameType s)) => Ord (ScopedIden' s)

deriving stock instance
  (Show (RefNameType s)) => Show (ScopedIden' s)

identifierName :: forall n. SingI n => ScopedIden' n -> RefNameType n
identifierName = \case
  ScopedAxiom a -> _axiomRefName a
  ScopedInductive i -> _inductiveRefName i
  ScopedVar v ->
    ( case sing :: S.SIsConcrete n of
        S.SConcrete -> id
        S.SNotConcrete -> set S.nameConcrete ()
    )
      (unqualifiedSymbol v)
  ScopedFunction f -> _functionRefName f
  ScopedConstructor c -> _constructorRefName c

data Expression
  = ExpressionIdentifier ScopedIden
  | ExpressionParensIdentifier ScopedIden
  | ExpressionApplication Application
  | ExpressionInfixApplication InfixApplication
  | ExpressionPostfixApplication PostfixApplication
  | ExpressionLambda (Lambda 'Scoped)
  | ExpressionMatch (Match 'Scoped)
  | ExpressionLetBlock (LetBlock 'Scoped)
  | ExpressionUniverse Universe
  | ExpressionLiteral Literal
  | ExpressionFunction (Function 'Scoped)
  deriving stock (Show, Eq, Ord)

instance HasAtomicity Literal where
  atomicity = \case
    LitInteger {} -> Atom
    LitString {} -> Atom

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIdentifier {} -> Atom
    ExpressionParensIdentifier {} -> Atom
    ExpressionApplication {} -> Aggregate appFixity
    ExpressionInfixApplication a -> Aggregate (getFixity a)
    ExpressionPostfixApplication a -> Aggregate (getFixity a)
    ExpressionLambda {} -> Atom
    ExpressionLiteral {} -> Atom
    ExpressionMatch {} -> Atom
    ExpressionLetBlock {} -> Atom
    ExpressionUniverse {} -> Atom
    ExpressionFunction {} -> Aggregate funFixity

--------------------------------------------------------------------------------
-- Expression atom
--------------------------------------------------------------------------------

data Literal
  = LitString Text
  | LitInteger Integer
  deriving stock (Show, Eq, Ord)

-- | Expressions without application
data ExpressionAtom (s :: Stage)
  = AtomIdentifier (IdentifierType s)
  | AtomLambda (Lambda s)
  | AtomLetBlock (LetBlock s)
  | AtomUniverse Universe
  | AtomFunction (Function s)
  | AtomFunArrow
  | AtomMatch (Match s)
  | AtomLiteral Literal
  | AtomParens (ExpressionType s)

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (PatternType s)
  ) =>
  Show (ExpressionAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (PatternType s)
  ) =>
  Eq (ExpressionAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (PatternType s)
  ) =>
  Ord (ExpressionAtom s)

-- | Expressions without application
newtype ExpressionAtoms (s :: Stage)
  = ExpressionAtoms (NonEmpty (ExpressionAtom s))

instance HasAtomicity (ExpressionAtoms 'Parsed) where
  atomicity (ExpressionAtoms l) = case l of
    (_ :| []) -> Atom
    (_ :| _)
      | AtomFunArrow `elem` l -> Aggregate funFixity
      | otherwise -> Aggregate appFixity

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (PatternType s)
  ) =>
  Show (ExpressionAtoms s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (PatternType s)
  ) =>
  Eq (ExpressionAtoms s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (PatternType s)
  ) =>
  Ord (ExpressionAtoms s)

--------------------------------------------------------------------------------
-- Match expression
--------------------------------------------------------------------------------

data MatchAlt (s :: Stage) = MatchAlt
  { matchAltPattern :: PatternType s,
    matchAltBody :: ExpressionType s
  }

deriving stock instance
  ( Show (ExpressionType s),
    Show (PatternType s)
  ) =>
  Show (MatchAlt s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (PatternType s)
  ) =>
  Eq (MatchAlt s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (PatternType s)
  ) =>
  Ord (MatchAlt s)

data Match (s :: Stage) = Match
  { matchExpression :: ExpressionType s,
    matchAlts :: [MatchAlt s]
  }

deriving stock instance
  ( Show (ExpressionType s),
    Show (PatternType s)
  ) =>
  Show (Match s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (PatternType s)
  ) =>
  Eq (Match s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (PatternType s)
  ) =>
  Ord (Match s)

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

data FunctionParameter (s :: Stage) = FunctionParameter
  { paramName :: Maybe (SymbolType s),
    paramUsage :: Maybe Usage,
    paramType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (FunctionParameter s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (FunctionParameter s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (FunctionParameter s)

data Function (s :: Stage) = Function
  { funParameter :: FunctionParameter s,
    funReturn :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (Function s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (Function s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (Function s)

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock (s :: Stage) = WhereBlock
  { whereClauses :: NonEmpty (WhereClause s)
  }

deriving stock instance
  ( Show (PatternType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (WhereBlock s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (WhereBlock s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (WhereBlock s)

data WhereClause (s :: Stage)
  = WhereOpenModule (OpenModule s)
  | WhereTypeSig (TypeSignature s)
  | WhereFunClause (FunctionClause s)

deriving stock instance
  ( Show (PatternType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (WhereClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (WhereClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (WhereClause s)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

-- Notes: An empty lambda, here called 'the impossible case', is a lambda
-- expression with empty list of arguments and empty body.

newtype Lambda (s :: Stage) = Lambda
  {lambdaClauses :: [LambdaClause s]}

deriving stock instance
  ( Show (PatternType s),
    Show (ExpressionType s)
  ) =>
  Show (Lambda s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Lambda s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Lambda s)

data LambdaClause (s :: Stage) = LambdaClause
  { lambdaParameters :: NonEmpty (PatternType s),
    lambdaBody :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternType s),
    Show (ExpressionType s)
  ) =>
  Show (LambdaClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LambdaClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LambdaClause s)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application = Application
  { applicationFunction :: ExpressionType 'Scoped,
    applicationParameter :: ExpressionType 'Scoped
  }
  deriving stock (Show, Eq, Ord)

data InfixApplication = InfixApplication
  { infixAppLeft :: ExpressionType 'Scoped,
    infixAppOperator :: IdentifierType 'Scoped,
    infixAppRight :: ExpressionType 'Scoped
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity InfixApplication where
  getFixity (InfixApplication _ op _) = fromMaybe impossible (identifierName op ^. S.nameFixity)

data PostfixApplication = PostfixApplication
  { postfixAppParameter :: ExpressionType 'Scoped,
    postfixAppOperator :: IdentifierType 'Scoped
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PostfixApplication where
  getFixity (PostfixApplication _ op) = fromMaybe impossible (identifierName op ^. S.nameFixity)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

data LetBlock (s :: Stage) = LetBlock
  { letClauses :: [LetClause s],
    letExpression :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (LetBlock s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LetBlock s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LetBlock s)

data LetClause (s :: Stage)
  = LetTypeSig (TypeSignature s)
  | LetFunClause (FunctionClause s)

deriving stock instance
  ( Show (PatternType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (LetClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LetClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LetClause s)

--------------------------------------------------------------------------------
-- Backends
--------------------------------------------------------------------------------

data Backend = BackendGhc | BackendAgda
  deriving stock (Show, Eq, Ord)

data BackendItem = BackendItem
  { _backendItemBackend :: Backend,
    _backendItemCode :: Text
  }
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

newtype Eval (s :: Stage) = Eval {evalExpression :: ExpressionType s}

deriving stock instance
  Show
    (ExpressionType s) =>
  Show (Eval s)

deriving stock instance
  Eq
    (ExpressionType s) =>
  Eq (Eval s)

deriving stock instance
  Ord
    (ExpressionType s) =>
  Ord (Eval s)

newtype Print (s :: Stage) = Print {printExpression :: ExpressionType s}

deriving stock instance
  Show
    ( ExpressionType s
    ) =>
  Show (Print s)

deriving stock instance
  Eq
    (ExpressionType s) =>
  Eq (Print s)

deriving stock instance
  Ord
    (ExpressionType s) =>
  Ord (Print s)

makeLenses ''InductiveDef
makeLenses ''Module
makeLenses ''TypeSignature
makeLenses ''AxiomDef
makeLenses ''FunctionClause
makeLenses ''InductiveParameter
makeLenses ''ForeignBlock
makeLenses ''AxiomRef'
makeLenses ''InductiveRef'
makeLenses ''ModuleRef'
makeLenses ''ModuleRef''
makeLenses ''FunctionRef'
makeLenses ''ConstructorRef'
makeLenses ''BackendItem

idenOverName :: (forall s. S.Name' s -> S.Name' s) -> ScopedIden -> ScopedIden
idenOverName f = \case
  ScopedAxiom a -> ScopedAxiom (over axiomRefName f a)
  ScopedInductive i -> ScopedInductive (over inductiveRefName f i)
  ScopedVar v -> ScopedVar (f v)
  ScopedFunction fun -> ScopedFunction (over functionRefName f fun)
  ScopedConstructor c -> ScopedConstructor (over constructorRefName f c)

entryPrism :: (S.Name' () -> S.Name' ()) -> SymbolEntry -> (S.Name' (), SymbolEntry)
entryPrism f = \case
  EntryAxiom a -> (a ^. axiomRefName, EntryAxiom (over axiomRefName f a))
  EntryInductive i -> (i ^. inductiveRefName, EntryInductive (over inductiveRefName f i))
  EntryFunction fun -> (fun ^. functionRefName, EntryFunction (over functionRefName f fun))
  EntryConstructor c -> (c ^. constructorRefName, EntryConstructor (over constructorRefName f c))
  EntryModule m -> (getModuleRefNameType m, EntryModule (overModuleRef'' (over moduleRefName f) m))

entryOverName :: (S.Name' () -> S.Name' ()) -> SymbolEntry -> SymbolEntry
entryOverName f = snd . entryPrism f

entryName :: SymbolEntry -> S.Name' ()
entryName = fst . entryPrism id

overModuleRef'' :: forall s s'. (forall t. ModuleRef'' s t -> ModuleRef'' s' t) -> ModuleRef' s -> ModuleRef' s'
overModuleRef'' f = over unModuleRef' (\(t :&: m'') -> t :&: f m'')

symbolEntryToSName :: SymbolEntry -> S.Name' ()
symbolEntryToSName = \case
  EntryAxiom a -> a ^. axiomRefName
  EntryInductive i -> i ^. inductiveRefName
  EntryFunction f -> f ^. functionRefName
  EntryConstructor c -> c ^. constructorRefName
  EntryModule m -> getModuleRefNameType m

instance HasNameKind SymbolEntry where
  getNameKind = getNameKind . entryName
