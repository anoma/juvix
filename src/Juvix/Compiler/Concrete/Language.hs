{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Concrete.Language
  ( module Juvix.Compiler.Concrete.Language,
    module Juvix.Compiler.Concrete.Data.Name,
    module Juvix.Compiler.Concrete.Data.NameRef,
    module Juvix.Data.Keyword,
    module Juvix.Compiler.Concrete.Data.Builtins,
    module Juvix.Compiler.Concrete.Data.Literal,
    module Juvix.Data,
    module Juvix.Compiler.Concrete.Data.VisibilityAnn,
    module Juvix.Compiler.Concrete.Data.PublicAnn,
    module Juvix.Compiler.Concrete.Data.ModuleIsTop,
    module Juvix.Data.Fixity,
  )
where

import Data.Kind qualified as GHC
import Juvix.Compiler.Concrete.Data.Builtins
import Juvix.Compiler.Concrete.Data.Literal
import Juvix.Compiler.Concrete.Data.ModuleIsTop
import Juvix.Compiler.Concrete.Data.Name
import Juvix.Compiler.Concrete.Data.NameRef
import Juvix.Compiler.Concrete.Data.NameSignature.Base (NameSignature)
import Juvix.Compiler.Concrete.Data.PublicAnn
import Juvix.Compiler.Concrete.Data.ScopedName (unqualifiedSymbol)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Data
import Juvix.Data.Ape.Base as Ape
import Juvix.Data.Fixity
import Juvix.Data.IteratorAttribs
import Juvix.Data.Keyword
import Juvix.Data.NameKind
import Juvix.Parser.Lexer (isDelimiterStr)
import Juvix.Prelude hiding (show)
import Juvix.Prelude.Pretty (prettyText)
import Prelude (show)

data Stage
  = Parsed
  | Scoped
  deriving stock (Eq, Show)

type AnyStage (k :: Stage -> GHC.Type) =
  Σ Stage (TyCon1 k)

$(genSingletons [''Stage])

type Delims = Irrelevant (Maybe (KeywordRef, KeywordRef))

--------------------------------------------------------------------------------
-- Parsing stages
--------------------------------------------------------------------------------

type SymbolType :: Stage -> GHC.Type
type family SymbolType s = res | res -> s where
  SymbolType 'Parsed = Symbol
  SymbolType 'Scoped = S.Symbol

type ModuleRefType :: Stage -> GHC.Type
type family ModuleRefType s = res | res -> s where
  ModuleRefType 'Parsed = Name
  ModuleRefType 'Scoped = ModuleRef

type IdentifierType :: Stage -> GHC.Type
type family IdentifierType s = res | res -> s where
  IdentifierType 'Parsed = Name
  IdentifierType 'Scoped = ScopedIden

type HoleType :: Stage -> GHC.Type
type family HoleType s = res | res -> s where
  HoleType 'Parsed = KeywordRef
  HoleType 'Scoped = Hole

type PatternAtomIdenType :: Stage -> GHC.Type
type family PatternAtomIdenType s = res | res -> s where
  PatternAtomIdenType 'Parsed = Name
  PatternAtomIdenType 'Scoped = PatternScopedIden

type ExpressionType :: Stage -> GHC.Type
type family ExpressionType s = res | res -> s where
  ExpressionType 'Parsed = ExpressionAtoms 'Parsed
  ExpressionType 'Scoped = Expression

type PatternAtomType :: Stage -> GHC.Type
type family PatternAtomType s = res | res -> s where
  PatternAtomType 'Parsed = PatternAtom 'Parsed
  PatternAtomType 'Scoped = PatternArg

type PatternParensType :: Stage -> GHC.Type
type family PatternParensType s = res | res -> s where
  PatternParensType 'Parsed = PatternAtoms 'Parsed
  PatternParensType 'Scoped = PatternArg

type PatternAtType :: Stage -> GHC.Type
type family PatternAtType s = res | res -> s where
  PatternAtType 'Parsed = PatternBinding
  PatternAtType 'Scoped = PatternArg

type ImportType :: Stage -> GHC.Type
type family ImportType s = res | res -> s where
  ImportType 'Parsed = TopModulePath
  ImportType 'Scoped = ModuleRef'' 'S.Concrete 'ModuleTop

type NameSignatureType :: Stage -> GHC.Type
type family NameSignatureType s = res | res -> s where
  NameSignatureType 'Parsed = ()
  NameSignatureType 'Scoped = NameSignature

type AmbiguousIteratorType :: Stage -> GHC.Type
type family AmbiguousIteratorType s = res | res -> s where
  AmbiguousIteratorType 'Parsed = AmbiguousIterator
  AmbiguousIteratorType 'Scoped = Void

type ModulePathType :: Stage -> ModuleIsTop -> GHC.Type
type family ModulePathType s t = res | res -> t s where
  ModulePathType 'Parsed 'ModuleTop = TopModulePath
  ModulePathType 'Scoped 'ModuleTop = S.TopModulePath
  ModulePathType 'Parsed 'ModuleLocal = Symbol
  ModulePathType 'Scoped 'ModuleLocal = S.Symbol

type ModuleEndType :: ModuleIsTop -> GHC.Type
type family ModuleEndType t = res | res -> t where
  ModuleEndType 'ModuleTop = ()
  ModuleEndType 'ModuleLocal = KeywordRef

--------------------------------------------------------------------------------
-- Pragmas
--------------------------------------------------------------------------------

-- We keep the exact source of the pragma text. This is necessary, because
-- pragmas are supposed to be backwards-compatible. Unrecognised pragmas
-- should be ignored, but they still need to be printed out when
-- pretty-printing. Also, we probably don't want to impose pragma formatting
-- choices on the user.
type ParsedPragmas = WithLoc (WithSource Pragmas)

--------------------------------------------------------------------------------
-- Iterator attributes
--------------------------------------------------------------------------------

type ParsedIteratorAttribs = WithLoc (WithSource IteratorAttribs)

--------------------------------------------------------------------------------
-- Top level statement
--------------------------------------------------------------------------------

-- | We group consecutive definitions and reserve symbols in advance, so that we
-- don't need extra syntax for mutually recursive definitions. Also, it allows
-- us to be more flexible with the ordering of the definitions.
data StatementSections (s :: Stage)
  = SectionsDefinitions (DefinitionsSection s)
  | SectionsNonDefinitions (NonDefinitionsSection s)
  | SectionsEmpty

data DefinitionsSection (s :: Stage) = DefinitionsSection
  { _definitionsSection :: NonEmpty (Definition s),
    _definitionsNext :: Maybe (NonDefinitionsSection s)
  }

data NonDefinitionsSection (s :: Stage) = NonDefinitionsSection
  { _nonDefinitionsSection :: NonEmpty (NonDefinition s),
    _nonDefinitionsNext :: Maybe (DefinitionsSection s)
  }

data Definition (s :: Stage)
  = DefinitionSyntax SyntaxDef
  | DefinitionFunctionDef (FunctionDef s)
  | DefinitionInductive (InductiveDef s)
  | DefinitionAxiom (AxiomDef s)
  | DefinitionTypeSignature (TypeSignature s)

data NonDefinition (s :: Stage)
  = NonDefinitionImport (Import s)
  | NonDefinitionModule (Module s 'ModuleLocal)
  | NonDefinitionFunctionClause (FunctionClause s)
  | NonDefinitionOpenModule (OpenModule s)

data Statement (s :: Stage)
  = StatementSyntax SyntaxDef
  | StatementTypeSignature (TypeSignature s)
  | StatementFunctionDef (FunctionDef s)
  | StatementImport (Import s)
  | StatementInductive (InductiveDef s)
  | StatementModule (Module s 'ModuleLocal)
  | StatementOpenModule (OpenModule s)
  | StatementFunctionClause (FunctionClause s)
  | StatementAxiom (AxiomDef s)

deriving stock instance
  ( Show (ImportType s),
    Show (ModulePathType s 'ModuleLocal),
    Show (ModulePathType s 'ModuleTop),
    Show (PatternAtomType s),
    Show (SymbolType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (ExpressionType s)
  ) =>
  Show (Statement s)

deriving stock instance
  ( Eq (ImportType s),
    Eq (PatternAtomType s),
    Eq (ModulePathType s 'ModuleLocal),
    Eq (ModulePathType s 'ModuleTop),
    Eq (SymbolType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Statement s)

deriving stock instance
  ( Ord (ImportType s),
    Ord (PatternAtomType s),
    Ord (ModulePathType s 'ModuleLocal),
    Ord (ModulePathType s 'ModuleTop),
    Ord (SymbolType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Statement s)

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

data Import (s :: Stage) = Import
  { _importKw :: KeywordRef,
    _importModule :: ImportType s,
    _importAsName :: Maybe (ModulePathType s 'ModuleTop)
  }

deriving stock instance (Show (ModulePathType s 'ModuleTop), Show (ImportType s)) => Show (Import s)

deriving stock instance (Eq (ModulePathType s 'ModuleTop), Eq (ImportType s)) => Eq (Import s)

deriving stock instance (Ord (ModulePathType s 'ModuleTop), Ord (ImportType s)) => Ord (Import s)

--------------------------------------------------------------------------------
-- Syntax declaration
--------------------------------------------------------------------------------

data SyntaxDef
  = SyntaxOperator OperatorSyntaxDef
  | SyntaxIterator IteratorSyntaxDef
  deriving stock (Show, Eq, Ord)

instance HasLoc SyntaxDef where
  getLoc = \case
    SyntaxOperator t -> getLoc t
    SyntaxIterator t -> getLoc t

--------------------------------------------------------------------------------
-- Operator syntax declaration
--------------------------------------------------------------------------------

data OperatorSyntaxDef = OperatorSyntaxDef
  { _opSymbol :: Symbol,
    _opFixity :: Fixity,
    _opKw :: KeywordRef,
    _opSyntaxKw :: KeywordRef
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc OperatorSyntaxDef where
  getLoc OperatorSyntaxDef {..} = getLoc _opSyntaxKw <> getLoc _opSymbol

--------------------------------------------------------------------------------
-- Iterator syntax declaration
--------------------------------------------------------------------------------

data IteratorSyntaxDef = IteratorSyntaxDef
  { _iterSymbol :: Symbol,
    _iterAttribs :: Maybe ParsedIteratorAttribs,
    _iterSyntaxKw :: KeywordRef,
    _iterIteratorKw :: KeywordRef
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc IteratorSyntaxDef where
  getLoc IteratorSyntaxDef {..} = getLoc _iterSyntaxKw <> getLoc _iterSymbol

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data SigArg (s :: Stage) = SigArg
  { _sigArgDelims :: Irrelevant (KeywordRef, KeywordRef),
    _sigArgImplicit :: IsImplicit,
    _sigArgColon :: Irrelevant KeywordRef,
    _sigArgNames :: NonEmpty (SymbolType s),
    _sigArgType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (SigArg s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (SigArg s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (SigArg s)

data NewFunctionClause (s :: Stage) = NewFunctionClause
  { _clausenPipeKw :: Irrelevant KeywordRef,
    _clausenPatterns :: NonEmpty (PatternAtomType s),
    _clausenAssignKw :: Irrelevant KeywordRef,
    _clausenBody :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (NewFunctionClause s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (NewFunctionClause s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (NewFunctionClause s)

data FunctionDefBody (s :: Stage)
  = SigBodyExpression (ExpressionType s)
  | SigBodyClauses (NonEmpty (NewFunctionClause s))

deriving stock instance
  ( Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (FunctionDefBody s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (FunctionDefBody s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (FunctionDefBody s)

data FunctionDef (s :: Stage) = FunctionDef
  { _signName :: FunctionName s,
    _signArgs :: [SigArg s],
    _signColonKw :: Irrelevant KeywordRef,
    _signRetType :: ExpressionType s,
    _signDoc :: Maybe (Judoc s),
    _signPragmas :: Maybe ParsedPragmas,
    _signBuiltin :: Maybe (WithLoc BuiltinFunction),
    _signBody :: FunctionDefBody s,
    _signTerminating :: Maybe KeywordRef
  }

deriving stock instance
  ( Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (FunctionDef s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (FunctionDef s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (FunctionDef s)

data TypeSignature (s :: Stage) = TypeSignature
  { _sigName :: FunctionName s,
    _sigColonKw :: Irrelevant KeywordRef,
    _sigType :: ExpressionType s,
    _sigDoc :: Maybe (Judoc s),
    _sigAssignKw :: Irrelevant (Maybe KeywordRef),
    _sigPragmas :: Maybe ParsedPragmas,
    _sigBuiltin :: Maybe (WithLoc BuiltinFunction),
    _sigBody :: Maybe (ExpressionType s),
    _sigTerminating :: Maybe KeywordRef
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (TypeSignature s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (TypeSignature s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (TypeSignature s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef (s :: Stage) = AxiomDef
  { _axiomKw :: Irrelevant KeywordRef,
    _axiomDoc :: Maybe (Judoc s),
    _axiomPragmas :: Maybe ParsedPragmas,
    _axiomName :: SymbolType s,
    _axiomColonKw :: Irrelevant KeywordRef,
    _axiomBuiltin :: Maybe (WithLoc BuiltinAxiom),
    _axiomType :: ExpressionType s
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
  { _constructorPipe :: Irrelevant (Maybe KeywordRef),
    _constructorColonKw :: Irrelevant KeywordRef,
    _constructorName :: InductiveConstructorName s,
    _constructorDoc :: Maybe (Judoc s),
    _constructorPragmas :: Maybe ParsedPragmas,
    _constructorType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveConstructorDef s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveConstructorDef s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveConstructorDef s)

data InductiveParameters (s :: Stage) = InductiveParameters
  { _inductiveParametersNames :: NonEmpty (SymbolType s),
    _inductiveParametersType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveParameters s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveParameters s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveParameters s)

data InductiveDef (s :: Stage) = InductiveDef
  { _inductiveKw :: Irrelevant KeywordRef,
    _inductiveAssignKw :: Irrelevant KeywordRef,
    _inductiveBuiltin :: Maybe (WithLoc BuiltinInductive),
    _inductiveDoc :: Maybe (Judoc s),
    _inductivePragmas :: Maybe ParsedPragmas,
    _inductiveName :: InductiveName s,
    _inductiveParameters :: [InductiveParameters s],
    _inductiveType :: Maybe (ExpressionType s),
    _inductiveConstructors :: NonEmpty (InductiveConstructorDef s),
    _inductivePositive :: Maybe KeywordRef
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveDef s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveDef s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveDef s)

--------------------------------------------------------------------------------
-- Pattern
--------------------------------------------------------------------------------

data PatternApp = PatternApp
  { _patAppLeft :: PatternArg,
    _patAppRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord)

data PatternInfixApp = PatternInfixApp
  { _patInfixLeft :: PatternArg,
    _patInfixConstructor :: S.Name,
    _patInfixRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PatternInfixApp where
  getFixity (PatternInfixApp _ op _) = fromMaybe impossible (op ^. S.nameFixity)

data PatternPostfixApp = PatternPostfixApp
  { _patPostfixParameter :: PatternArg,
    _patPostfixConstructor :: S.Name
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PatternPostfixApp where
  getFixity (PatternPostfixApp _ op) = fromMaybe impossible (op ^. S.nameFixity)

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgName :: Maybe S.Symbol,
    _patternArgPattern :: Pattern
  }
  deriving stock (Show, Eq, Ord)

data Pattern
  = PatternVariable (SymbolType 'Scoped)
  | PatternConstructor S.Name
  | PatternApplication PatternApp
  | PatternList (ListPattern 'Scoped)
  | PatternInfixApplication PatternInfixApp
  | PatternPostfixApplication PatternPostfixApp
  | PatternWildcard Wildcard
  | PatternEmpty Interval
  deriving stock (Show, Eq, Ord)

instance HasAtomicity (ListPattern s) where
  atomicity = const Atom

instance HasAtomicity Pattern where
  atomicity e = case e of
    PatternVariable {} -> Atom
    PatternConstructor {} -> Atom
    PatternApplication {} -> Aggregate appFixity
    PatternInfixApplication a -> Aggregate (getFixity a)
    PatternPostfixApplication p -> Aggregate (getFixity p)
    PatternWildcard {} -> Atom
    PatternList l -> atomicity l
    PatternEmpty {} -> Atom

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternScopedIden
  = PatternScopedVar S.Symbol
  | PatternScopedConstructor S.Name
  deriving stock (Show, Ord, Eq)

data PatternBinding = PatternBinding
  { _patternBindingName :: Symbol,
    _patternBindingPattern :: PatternAtom 'Parsed
  }

data ListPattern (s :: Stage) = ListPattern
  { _listpBracketL :: Irrelevant KeywordRef,
    _listpBracketR :: Irrelevant KeywordRef,
    _listpItems :: [PatternParensType s]
  }

deriving stock instance (Show (PatternParensType s)) => Show (ListPattern s)

deriving stock instance (Eq (PatternParensType s)) => Eq (ListPattern s)

deriving stock instance (Ord (PatternParensType s)) => Ord (ListPattern s)

data PatternAtom (s :: Stage)
  = PatternAtomIden (PatternAtomIdenType s)
  | PatternAtomWildcard Wildcard
  | PatternAtomEmpty Interval
  | PatternAtomList (ListPattern s)
  | PatternAtomParens (PatternParensType s)
  | PatternAtomBraces (PatternParensType s)
  | PatternAtomAt (PatternAtType s)

data PatternAtoms (s :: Stage) = PatternAtoms
  { _patternAtoms :: NonEmpty (PatternAtom s),
    _patternAtomsLoc :: Interval
  }

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type FunctionName s = SymbolType s

data FunctionClause (s :: Stage) = FunctionClause
  { _clauseOwnerFunction :: FunctionName s,
    _clauseAssignKw :: Irrelevant KeywordRef,
    _clausePatterns :: [PatternAtomType s],
    _clauseBody :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (FunctionClause s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (FunctionClause s)

deriving stock instance
  ( Ord (PatternAtomType s),
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
  { _moduleKw :: KeywordRef,
    _modulePath :: ModulePathType s t,
    _moduleDoc :: Maybe (Judoc s),
    _modulePragmas :: Maybe ParsedPragmas,
    _moduleBody :: [Statement s],
    _moduleKwEnd :: ModuleEndType t
  }

deriving stock instance
  ( Show (ModulePathType s t),
    Show (ModulePathType s 'ModuleLocal),
    Show (ModulePathType s 'ModuleTop),
    Show (ImportType s),
    Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ModuleEndType t),
    Show (ExpressionType s)
  ) =>
  Show (Module s t)

deriving stock instance
  ( Eq (ModulePathType s t),
    Eq (ModulePathType s 'ModuleLocal),
    Eq (ModulePathType s 'ModuleTop),
    Eq (ImportType s),
    Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ModuleEndType t),
    Eq (ExpressionType s)
  ) =>
  Eq (Module s t)

deriving stock instance
  ( Ord (ModulePathType s t),
    Ord (ModulePathType s 'ModuleLocal),
    Ord (ModulePathType s 'ModuleTop),
    Ord (ImportType s),
    Ord (PatternAtomType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ModuleEndType t),
    Ord (ExpressionType s)
  ) =>
  Ord (Module s t)

newtype HidingItem (s :: Stage) = HidingItem
  { _hidingSymbol :: SymbolType s
  }

deriving stock instance
  ( Show (SymbolType s)
  ) =>
  Show (HidingItem s)

deriving stock instance
  ( Eq (SymbolType s)
  ) =>
  Eq (HidingItem s)

deriving stock instance
  ( Ord (SymbolType s)
  ) =>
  Ord (HidingItem s)

data UsingItem (s :: Stage) = UsingItem
  { _usingSymbol :: SymbolType s,
    _usingAsKw :: Irrelevant (Maybe KeywordRef),
    _usingAs :: Maybe (SymbolType s)
  }

deriving stock instance
  ( Show (SymbolType s)
  ) =>
  Show (UsingItem s)

deriving stock instance
  ( Eq (SymbolType s)
  ) =>
  Eq (UsingItem s)

deriving stock instance
  ( Ord (SymbolType s)
  ) =>
  Ord (UsingItem s)

data UsingList (s :: Stage) = UsingList
  { _usingKw :: Irrelevant KeywordRef,
    _usingBraces :: Irrelevant (KeywordRef, KeywordRef),
    _usingList :: NonEmpty (UsingItem s)
  }

deriving stock instance
  ( Show (SymbolType s)
  ) =>
  Show (UsingList s)

deriving stock instance
  ( Eq (SymbolType s)
  ) =>
  Eq (UsingList s)

deriving stock instance
  ( Ord (SymbolType s)
  ) =>
  Ord (UsingList s)

data HidingList (s :: Stage) = HidingList
  { _hidingKw :: Irrelevant KeywordRef,
    _hidingBraces :: Irrelevant (KeywordRef, KeywordRef),
    _hidingList :: NonEmpty (HidingItem s)
  }

deriving stock instance
  ( Show (SymbolType s)
  ) =>
  Show (HidingList s)

deriving stock instance
  ( Eq (SymbolType s)
  ) =>
  Eq (HidingList s)

deriving stock instance
  ( Ord (SymbolType s)
  ) =>
  Ord (HidingList s)

data UsingHiding (s :: Stage)
  = Using (UsingList s)
  | Hiding (HidingList s)

deriving stock instance
  ( Show (SymbolType s)
  ) =>
  Show (UsingHiding s)

deriving stock instance
  ( Eq (SymbolType s)
  ) =>
  Eq (UsingHiding s)

deriving stock instance
  ( Ord (SymbolType s)
  ) =>
  Ord (UsingHiding s)

type ModuleRef = ModuleRef' 'S.Concrete

newtype ModuleRef' (c :: S.IsConcrete) = ModuleRef'
  { _unModuleRef' :: Σ ModuleIsTop (TyCon1 (ModuleRef'' c))
  }

instance SingI c => Show (ModuleRef' c) where
  show (ModuleRef' (isTop :&: r)) = case isTop of
    SModuleLocal -> case sing :: S.SIsConcrete c of
      S.SConcrete -> show r
      S.SNotConcrete -> show r
    SModuleTop -> case sing :: S.SIsConcrete c of
      S.SConcrete -> show r
      S.SNotConcrete -> show r

getNameRefId :: forall c. (SingI c) => RefNameType c -> S.NameId
getNameRefId = case sing :: S.SIsConcrete c of
  S.SConcrete -> (^. S.nameId)
  S.SNotConcrete -> (^. S.nameId)

getModuleExportInfo :: ModuleRef' c -> ExportInfo
getModuleExportInfo (ModuleRef' (_ :&: ModuleRef'' {..})) = _moduleExportInfo

getModuleRefNameType :: ModuleRef' c -> RefNameType c
getModuleRefNameType (ModuleRef' (_ :&: ModuleRef'' {..})) = _moduleRefName

getModuleRefNameId :: forall c. SingI c => ModuleRef' c -> S.NameId
getModuleRefNameId (ModuleRef' (t :&: ModuleRef'' {..})) =
  case sing :: S.SIsConcrete c of
    S.SConcrete -> case t of
      SModuleTop -> _moduleRefName ^. S.nameId
      SModuleLocal -> _moduleRefName ^. S.nameId
    S.SNotConcrete -> _moduleRefName ^. S.nameId

instance SingI c => Eq (ModuleRef' c) where
  (==) = (==) `on` getModuleRefNameId

instance SingI c => Ord (ModuleRef' c) where
  compare = compare `on` getModuleRefNameId

data ModuleRef'' (c :: S.IsConcrete) (t :: ModuleIsTop) = ModuleRef''
  { _moduleRefName :: RefNameType c,
    _moduleExportInfo :: ExportInfo,
    _moduleRefModule :: Module 'Scoped t
  }

instance (Show (RefNameType s)) => Show (ModuleRef'' s t) where
  show ModuleRef'' {..} = show _moduleRefName

data SymbolEntry
  = EntryAxiom (RefNameType 'S.NotConcrete)
  | EntryInductive (RefNameType 'S.NotConcrete)
  | EntryFunction (RefNameType 'S.NotConcrete)
  | EntryConstructor (RefNameType 'S.NotConcrete)
  | EntryModule (ModuleRef' 'S.NotConcrete)
  | EntryVariable (S.Name' ())
  deriving stock (Show)

instance SingI t => CanonicalProjection (ModuleRef'' c t) (ModuleRef' c) where
  project r = ModuleRef' (sing :&: r)

-- | Symbols that a module exports
newtype ExportInfo = ExportInfo
  { _exportSymbols :: HashMap Symbol SymbolEntry
  }
  deriving stock (Show)

data OpenModule (s :: Stage) = OpenModule
  { _openModuleKw :: KeywordRef,
    _openModuleName :: ModuleRefType s,
    _openModuleImportKw :: Maybe KeywordRef,
    _openImportAsName :: Maybe (ModulePathType s 'ModuleTop),
    _openUsingHiding :: Maybe (UsingHiding s),
    _openPublicKw :: Irrelevant (Maybe KeywordRef),
    _openPublic :: PublicAnn
  }

deriving stock instance
  ( Eq (IdentifierType s),
    Eq (SymbolType s),
    Eq (ModuleRefType s),
    Eq (ModulePathType s 'ModuleTop),
    Eq (PatternAtomType s),
    Eq (ExpressionType s)
  ) =>
  Eq (OpenModule s)

deriving stock instance
  ( Ord (IdentifierType s),
    Ord (SymbolType s),
    Ord (PatternAtomType s),
    Ord (ModulePathType s 'ModuleTop),
    Ord (ModuleRefType s),
    Ord (ExpressionType s)
  ) =>
  Ord (OpenModule s)

deriving stock instance
  ( Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ModulePathType s 'ModuleTop),
    Show (ExpressionType s)
  ) =>
  Show (OpenModule s)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

type ScopedIden = ScopedIden' 'S.Concrete

data ScopedIden' (n :: S.IsConcrete)
  = ScopedAxiom (RefNameType n)
  | ScopedInductive (RefNameType n)
  | ScopedVar S.Symbol
  | ScopedFunction (RefNameType n)
  | ScopedConstructor (RefNameType n)

deriving stock instance
  (Eq (RefNameType s)) => Eq (ScopedIden' s)

deriving stock instance
  (Ord (RefNameType s)) => Ord (ScopedIden' s)

deriving stock instance
  (Show (RefNameType s)) => Show (ScopedIden' s)

identifierName :: forall n. (SingI n) => ScopedIden' n -> RefNameType n
identifierName = \case
  ScopedAxiom a -> a
  ScopedInductive i -> i
  ScopedVar v ->
    ( case sing :: S.SIsConcrete n of
        S.SConcrete -> id
        S.SNotConcrete -> set S.nameConcrete ()
    )
      (unqualifiedSymbol v)
  ScopedFunction f -> f
  ScopedConstructor c -> c

data Expression
  = ExpressionIdentifier ScopedIden
  | ExpressionParensIdentifier ScopedIden
  | ExpressionApplication Application
  | ExpressionInfixApplication InfixApplication
  | ExpressionPostfixApplication PostfixApplication
  | ExpressionList (List 'Scoped)
  | ExpressionCase (Case 'Scoped)
  | ExpressionLambda (Lambda 'Scoped)
  | ExpressionLet (Let 'Scoped)
  | ExpressionUniverse Universe
  | ExpressionLiteral LiteralLoc
  | ExpressionFunction (Function 'Scoped)
  | ExpressionHole (HoleType 'Scoped)
  | ExpressionBraces (WithLoc Expression)
  | ExpressionIterator (Iterator 'Scoped)
  | ExpressionNamedApplication (NamedApplication 'Scoped)
  deriving stock (Show, Eq, Ord)

instance HasAtomicity (Lambda s) where
  atomicity = const Atom

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------
data FunctionParameter (s :: Stage)
  = FunctionParameterName (SymbolType s)
  | FunctionParameterWildcard KeywordRef

deriving stock instance
  (Show (ExpressionType s), Show (SymbolType s)) =>
  Show (FunctionParameter s)

deriving stock instance
  (Eq (ExpressionType s), Eq (SymbolType s)) =>
  Eq (FunctionParameter s)

deriving stock instance
  (Ord (ExpressionType s), Ord (SymbolType s)) =>
  Ord (FunctionParameter s)

data FunctionParameters (s :: Stage) = FunctionParameters
  { _paramNames :: [FunctionParameter s],
    _paramImplicit :: IsImplicit,
    _paramDelims :: Delims,
    _paramColon :: Irrelevant (Maybe KeywordRef),
    _paramType :: ExpressionType s
  }

deriving stock instance
  (Show (ExpressionType s), Show (SymbolType s)) =>
  Show (FunctionParameters s)

deriving stock instance
  (Eq (ExpressionType s), Eq (SymbolType s)) =>
  Eq (FunctionParameters s)

deriving stock instance
  (Ord (ExpressionType s), Ord (SymbolType s)) =>
  Ord (FunctionParameters s)

-- | Function *type* representation
data Function (s :: Stage) = Function
  { _funParameters :: FunctionParameters s,
    _funKw :: KeywordRef,
    _funReturn :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (Function s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (Function s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (Function s)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

data Lambda (s :: Stage) = Lambda
  { _lambdaKw :: KeywordRef,
    _lambdaBraces :: Irrelevant (KeywordRef, KeywordRef),
    _lambdaClauses :: NonEmpty (LambdaClause s)
  }

deriving stock instance
  ( Show (PatternAtomType s),
    Show (ExpressionType s)
  ) =>
  Show (Lambda s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Lambda s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Lambda s)

data LambdaClause (s :: Stage) = LambdaClause
  { _lambdaPipe :: Irrelevant (Maybe KeywordRef),
    _lambdaParameters :: NonEmpty (PatternAtomType s),
    _lambdaAssignKw :: Irrelevant KeywordRef,
    _lambdaBody :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternAtomType s),
    Show (ExpressionType s)
  ) =>
  Show (LambdaClause s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LambdaClause s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LambdaClause s)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application = Application
  { _applicationFunction :: Expression,
    _applicationParameter :: Expression
  }
  deriving stock (Show, Eq, Ord)

data InfixApplication = InfixApplication
  { _infixAppLeft :: Expression,
    _infixAppOperator :: ScopedIden,
    _infixAppRight :: Expression
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity InfixApplication where
  getFixity (InfixApplication _ op _) = fromMaybe impossible (identifierName op ^. S.nameFixity)

data PostfixApplication = PostfixApplication
  { _postfixAppParameter :: Expression,
    _postfixAppOperator :: ScopedIden
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PostfixApplication where
  getFixity (PostfixApplication _ op) = fromMaybe impossible (identifierName op ^. S.nameFixity)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

data Let (s :: Stage) = Let
  { _letKw :: KeywordRef,
    _letInKw :: Irrelevant KeywordRef,
    _letClauses :: NonEmpty (LetClause s),
    _letExpression :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (Let s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Let s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Let s)

data LetClause (s :: Stage)
  = LetTypeSig (TypeSignature s)
  | LetFunClause (FunctionClause s)

deriving stock instance
  ( Show (PatternAtomType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (LetClause s)

deriving stock instance
  ( Eq (PatternAtomType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LetClause s)

deriving stock instance
  ( Ord (PatternAtomType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LetClause s)

data CaseBranch (s :: Stage) = CaseBranch
  { _caseBranchPipe :: Irrelevant KeywordRef,
    _caseBranchAssignKw :: Irrelevant KeywordRef,
    _caseBranchPattern :: PatternParensType s,
    _caseBranchExpression :: ExpressionType s
  }

deriving stock instance (Eq (ExpressionType s), Eq (PatternParensType s)) => Eq (CaseBranch s)

deriving stock instance (Show (ExpressionType s), Show (PatternParensType s)) => Show (CaseBranch s)

deriving stock instance (Ord (ExpressionType s), Ord (PatternParensType s)) => Ord (CaseBranch s)

data Case (s :: Stage) = Case
  { _caseKw :: KeywordRef,
    -- | Due to limitations of the pretty printing algorithm, we store whether
    -- the `case` was surrounded by parentheses in the code.
    _caseParens :: Bool,
    _caseExpression :: ExpressionType s,
    _caseBranches :: NonEmpty (CaseBranch s)
  }

deriving stock instance (Eq (ExpressionType s), Eq (PatternParensType s)) => Eq (Case s)

deriving stock instance (Show (ExpressionType s), Show (PatternParensType s)) => Show (Case s)

deriving stock instance (Ord (ExpressionType s), Ord (PatternParensType s)) => Ord (Case s)

--------------------------------------------------------------------------------
-- Initializer expression
--------------------------------------------------------------------------------

data Initializer (s :: Stage) = Initializer
  { _initializerPattern :: PatternParensType s,
    _initializerAssignKw :: Irrelevant KeywordRef,
    _initializerExpression :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternParensType s),
    Show (ExpressionType s)
  ) =>
  Show (Initializer s)

deriving stock instance
  ( Eq (PatternParensType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Initializer s)

deriving stock instance
  ( Ord (PatternParensType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Initializer s)

--------------------------------------------------------------------------------
-- Range expression
--------------------------------------------------------------------------------

data Range (s :: Stage) = Range
  { _rangePattern :: PatternParensType s,
    _rangeInKw :: Irrelevant KeywordRef,
    _rangeExpression :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternParensType s),
    Show (ExpressionType s)
  ) =>
  Show (Range s)

deriving stock instance
  ( Eq (PatternParensType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Range s)

deriving stock instance
  ( Ord (PatternParensType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Range s)

--------------------------------------------------------------------------------
-- Iterator/mapper expression
--------------------------------------------------------------------------------

data Iterator s = Iterator
  { _iteratorName :: IdentifierType s,
    _iteratorInitializers :: [Initializer s],
    _iteratorRanges :: [Range s],
    _iteratorBody :: ExpressionType s,
    -- | Was the body enclosed in braces?
    _iteratorBodyBraces :: Bool,
    -- | Due to limitations of the pretty printing algorithm, we store whether
    -- the iterator was surrounded by parentheses in the code.
    _iteratorParens :: Bool
  }

deriving stock instance
  ( Show (Initializer s),
    Show (Range s),
    Show (ExpressionType s),
    Show (IdentifierType s)
  ) =>
  Show (Iterator s)

deriving stock instance
  ( Eq (Initializer s),
    Eq (Range s),
    Eq (ExpressionType s),
    Eq (IdentifierType s)
  ) =>
  Eq (Iterator s)

deriving stock instance
  ( Ord (Initializer s),
    Ord (Range s),
    Ord (ExpressionType s),
    Ord (IdentifierType s)
  ) =>
  Ord (Iterator s)

-- | Either an Iterator or a NamedApplication. Has the form:
-- f (sym := expr; .. ; symn := expr) -- not followed by range
data AmbiguousIterator = AmbiguousIterator
  { _ambiguousIteratorName :: Name,
    _ambiguousIteratorInitializers :: NonEmpty (NamedArgument 'Parsed),
    _ambiguousIteratorBody :: ExpressionType 'Parsed,
    -- | Was the body enclosed in braces?
    _ambiguousIteratorBodyBraces :: Bool,
    -- | Due to limitations of the pretty printing algorithm, we store whether
    -- the iterator was surrounded by parentheses in the code.
    _ambiguousIteratorParens :: Bool
  }

deriving stock instance
  ( Show (PatternAtoms 'Parsed),
    Show (PatternAtom 'Parsed)
  ) =>
  Show (AmbiguousIterator)

deriving stock instance
  ( Eq (PatternAtoms 'Parsed),
    Eq (ExpressionAtoms 'Parsed)
  ) =>
  Eq (AmbiguousIterator)

deriving stock instance
  ( Ord (ExpressionAtoms 'Parsed),
    Ord (PatternAtoms 'Parsed)
  ) =>
  Ord (AmbiguousIterator)

data List (s :: Stage) = List
  { _listBracketL :: Irrelevant KeywordRef,
    _listBracketR :: Irrelevant KeywordRef,
    _listItems :: [ExpressionType s]
  }

deriving stock instance
  ( Show (ExpressionType s)
  ) =>
  Show (List s)

deriving stock instance
  ( Eq (ExpressionType s)
  ) =>
  Eq (List s)

deriving stock instance
  ( Ord (ExpressionType s)
  ) =>
  Ord (List s)

data NamedArgument (s :: Stage) = NamedArgument
  { _namedArgName :: Symbol,
    _namedArgAssignKw :: Irrelevant KeywordRef,
    _namedArgValue :: ExpressionType s
  }

deriving stock instance
  ( Show (ExpressionType s),
    Show (SymbolType s)
  ) =>
  Show (NamedArgument s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (SymbolType s)
  ) =>
  Eq (NamedArgument s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (SymbolType s)
  ) =>
  Ord (NamedArgument s)

data ArgumentBlock (s :: Stage) = ArgumentBlock
  { _argBlockDelims :: Irrelevant (Maybe (KeywordRef, KeywordRef)),
    _argBlockImplicit :: IsImplicit,
    _argBlockArgs :: NonEmpty (NamedArgument s)
  }

deriving stock instance
  ( Show (ExpressionType s),
    Show (SymbolType s)
  ) =>
  Show (ArgumentBlock s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (SymbolType s)
  ) =>
  Eq (ArgumentBlock s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (SymbolType s)
  ) =>
  Ord (ArgumentBlock s)

data NamedApplication (s :: Stage) = NamedApplication
  { _namedAppName :: IdentifierType s,
    _namedAppArgs :: NonEmpty (ArgumentBlock s),
    _namedAppSignature :: Irrelevant (NameSignatureType s)
  }

deriving stock instance
  ( Show (ExpressionType s),
    Show (SymbolType s),
    Show (NameSignatureType s),
    Show (IdentifierType s)
  ) =>
  Show (NamedApplication s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (SymbolType s),
    Eq (IdentifierType s)
  ) =>
  Eq (NamedApplication s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (SymbolType s),
    Ord (IdentifierType s)
  ) =>
  Ord (NamedApplication s)

-- | Expressions without application
data ExpressionAtom (s :: Stage)
  = AtomIdentifier (IdentifierType s)
  | AtomLambda (Lambda s)
  | AtomList (List s)
  | AtomCase (Case s)
  | AtomHole (HoleType s)
  | AtomBraces (WithLoc (ExpressionType s))
  | AtomLet (Let s)
  | AtomUniverse Universe
  | AtomFunction (Function s)
  | AtomFunArrow KeywordRef
  | AtomLiteral LiteralLoc
  | AtomParens (ExpressionType s)
  | AtomIterator (Iterator s)
  | AtomAmbiguousIterator (AmbiguousIteratorType s)
  | AtomNamedApplication (NamedApplication s)

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (NameSignatureType s),
    Show (ModuleRefType s),
    Show (AmbiguousIteratorType s),
    Show (HoleType s),
    Show (SymbolType s),
    Show (PatternParensType s),
    Show (PatternAtomType s)
  ) =>
  Show (ExpressionAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (AmbiguousIteratorType s),
    Eq (HoleType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (PatternParensType s),
    Eq (PatternAtomType s)
  ) =>
  Eq (ExpressionAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (AmbiguousIteratorType s),
    Ord (HoleType s),
    Ord (SymbolType s),
    Ord (PatternParensType s),
    Ord (PatternAtomType s)
  ) =>
  Ord (ExpressionAtom s)

data ExpressionAtoms (s :: Stage) = ExpressionAtoms
  { _expressionAtoms :: NonEmpty (ExpressionAtom s),
    _expressionAtomsLoc :: Interval
  }

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (NameSignatureType s),
    Show (ModuleRefType s),
    Show (AmbiguousIteratorType s),
    Show (HoleType s),
    Show (SymbolType s),
    Show (PatternParensType s),
    Show (PatternAtomType s)
  ) =>
  Show (ExpressionAtoms s)

newtype Judoc (s :: Stage) = Judoc
  { _judocGroups :: NonEmpty (JudocGroup s)
  }
  deriving newtype (Semigroup)

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (Judoc s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (Judoc s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (Judoc s)

data Example (s :: Stage) = Example
  { _exampleId :: NameId,
    _exampleLoc :: Interval,
    _exampleExpression :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s)) => Show (Example s)

deriving stock instance (Eq (ExpressionType s)) => Eq (Example s)

deriving stock instance (Ord (ExpressionType s)) => Ord (Example s)

data JudocBlockParagraph (s :: Stage) = JudocBlockParagraph
  { _judocBlockParagraphStart :: KeywordRef,
    _judocBlockParagraphBlocks :: [JudocBlock s],
    _judocBlockParagraphEnd :: KeywordRef
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocBlockParagraph s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocBlockParagraph s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocBlockParagraph s)

data JudocGroup (s :: Stage)
  = JudocGroupBlock (JudocBlockParagraph s)
  | JudocGroupLines (NonEmpty (JudocBlock s))

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocGroup s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocGroup s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocGroup s)

data JudocBlock (s :: Stage)
  = JudocLines (NonEmpty (JudocLine s))
  | JudocExample (Example s)

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocBlock s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocBlock s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocBlock s)

data JudocLine (s :: Stage) = JudocLine
  { _judocLineDelim :: Maybe KeywordRef,
    _judocLineAtoms :: NonEmpty (WithLoc (JudocAtom s))
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocLine s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocLine s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocLine s)

data JudocAtom (s :: Stage)
  = JudocExpression (ExpressionType s)
  | JudocText Text

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocAtom s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocAtom s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocAtom s)

newtype ModuleIndex = ModuleIndex
  { _moduleIxModule :: Module 'Scoped 'ModuleTop
  }

makeLenses ''PatternArg
makeLenses ''List
makeLenses ''ListPattern
makeLenses ''UsingItem
makeLenses ''HidingItem
makeLenses ''HidingList
makeLenses ''UsingList
makeLenses ''JudocLine
makeLenses ''Example
makeLenses ''Lambda
makeLenses ''LambdaClause
makeLenses ''Judoc
makeLenses ''JudocBlockParagraph
makeLenses ''Function
makeLenses ''InductiveDef
makeLenses ''PostfixApplication
makeLenses ''InfixApplication
makeLenses ''Application
makeLenses ''Let
makeLenses ''FunctionParameters
makeLenses ''Import
makeLenses ''OperatorSyntaxDef
makeLenses ''IteratorSyntaxDef
makeLenses ''InductiveConstructorDef
makeLenses ''Module
makeLenses ''TypeSignature
makeLenses ''SigArg
makeLenses ''FunctionDef
makeLenses ''AxiomDef
makeLenses ''FunctionClause
makeLenses ''InductiveParameters
makeLenses ''ModuleRef'
makeLenses ''ModuleRef''
makeLenses ''OpenModule
makeLenses ''PatternApp
makeLenses ''PatternInfixApp
makeLenses ''PatternPostfixApp
makeLenses ''Case
makeLenses ''CaseBranch
makeLenses ''PatternBinding
makeLenses ''PatternAtoms
makeLenses ''ExpressionAtoms
makeLenses ''Iterator
makeLenses ''AmbiguousIterator
makeLenses ''Initializer
makeLenses ''Range
makeLenses ''ModuleIndex
makeLenses ''ArgumentBlock
makeLenses ''NamedArgument
makeLenses ''NamedApplication

instance Eq ModuleIndex where
  (==) = (==) `on` (^. moduleIxModule . modulePath)

instance Hashable ModuleIndex where
  hashWithSalt s = hashWithSalt s . (^. moduleIxModule . modulePath)

instance SingI s => HasLoc (NamedArgument s) where
  getLoc NamedArgument {..} = getLocSymbolType _namedArgName <> getLocExpressionType _namedArgValue

instance SingI s => HasLoc (ArgumentBlock s) where
  getLoc ArgumentBlock {..} = case d of
    Just (l, r) -> getLoc l <> getLoc r
    Nothing -> getLocSpan _argBlockArgs
    where
      Irrelevant d = _argBlockDelims

instance HasAtomicity (ArgumentBlock s) where
  atomicity = const Atom

instance HasAtomicity (NamedApplication s) where
  atomicity = const (Aggregate appFixity)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIdentifier {} -> Atom
    ExpressionHole {} -> Atom
    ExpressionParensIdentifier {} -> Atom
    ExpressionApplication {} -> Aggregate appFixity
    ExpressionInfixApplication a -> Aggregate (getFixity a)
    ExpressionPostfixApplication a -> Aggregate (getFixity a)
    ExpressionLambda l -> atomicity l
    ExpressionLiteral l -> atomicity l
    ExpressionLet l -> atomicity l
    ExpressionBraces {} -> Atom
    ExpressionList {} -> Atom
    ExpressionUniverse {} -> Atom
    ExpressionFunction {} -> Aggregate funFixity
    ExpressionCase c -> atomicity c
    ExpressionIterator i -> atomicity i
    ExpressionNamedApplication i -> atomicity i

expressionAtomicity :: forall s. SingI s => ExpressionType s -> Atomicity
expressionAtomicity e = case sing :: SStage s of
  SParsed -> atomicity e
  SScoped -> atomicity e

instance HasAtomicity (Iterator s) where
  atomicity = const Atom

instance HasAtomicity (Case s) where
  atomicity = const Atom

instance HasAtomicity (Let 'Scoped) where
  atomicity l = atomicity (l ^. letExpression)

instance Eq (ModuleRef'' 'S.Concrete t) where
  (==) = (==) `on` (^. moduleRefName)

instance HasAtomicity (PatternAtom 'Parsed) where
  atomicity = const Atom

instance SingI s => HasAtomicity (FunctionParameters s) where
  atomicity p
    | not (null (p ^. paramNames)) || p ^. paramImplicit == Implicit = Atom
    | otherwise = case sing :: SStage s of
        SParsed -> atomicity (p ^. paramType)
        SScoped -> atomicity (p ^. paramType)

deriving stock instance
  ( Show Symbol,
    Show (PatternAtom 'Parsed)
  ) =>
  Show PatternBinding

deriving stock instance
  ( Eq Symbol,
    Eq (PatternAtom 'Parsed)
  ) =>
  Eq PatternBinding

deriving stock instance
  ( Ord Symbol,
    Ord (PatternAtom 'Parsed)
  ) =>
  Ord PatternBinding

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (ListPattern s),
    Show (PatternAtomIdenType s),
    Show (PatternParensType s),
    Show (PatternAtType s),
    Show (PatternAtomType s)
  ) =>
  Show (PatternAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (ListPattern s),
    Eq (PatternAtomIdenType s),
    Eq (PatternParensType s),
    Eq (PatternAtType s),
    Eq (PatternAtomType s)
  ) =>
  Eq (PatternAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ListPattern s),
    Ord (PatternAtomIdenType s),
    Ord (PatternParensType s),
    Ord (PatternAtType s),
    Ord (PatternAtomType s)
  ) =>
  Ord (PatternAtom s)

deriving stock instance
  (Show (PatternAtom s)) => Show (PatternAtoms s)

instance HasLoc ScopedIden where
  getLoc = \case
    ScopedAxiom a -> getLoc a
    ScopedConstructor a -> getLoc a
    ScopedInductive a -> getLoc a
    ScopedFunction a -> getLoc a
    ScopedVar a -> getLoc a

instance HasLoc (InductiveDef 'Scoped) where
  getLoc i = (getLoc <$> i ^. inductivePositive) ?<> getLoc (i ^. inductiveKw)

instance HasLoc (FunctionClause 'Scoped) where
  getLoc c = getLoc (c ^. clauseOwnerFunction) <> getLoc (c ^. clauseBody)

instance HasLoc ModuleRef where
  getLoc (ModuleRef' (_ :&: r)) = getLoc r

instance HasLoc (AxiomDef 'Scoped) where
  getLoc m = getLoc (m ^. axiomKw) <> getLoc (m ^. axiomType)

instance HasLoc (OpenModule 'Scoped) where
  getLoc m =
    getLoc (m ^. openModuleKw)
      <> getLoc (m ^. openModuleName)
      <>? fmap getLoc (m ^. openPublicKw . unIrrelevant)

instance HasLoc (Statement 'Scoped) where
  getLoc :: Statement 'Scoped -> Interval
  getLoc = \case
    StatementSyntax t -> getLoc t
    StatementTypeSignature t -> getLoc t
    StatementFunctionDef t -> getLoc t
    StatementImport t -> getLoc t
    StatementInductive t -> getLoc t
    StatementModule t -> getLoc t
    StatementOpenModule t -> getLoc t
    StatementFunctionClause t -> getLoc t
    StatementAxiom t -> getLoc t

instance HasLoc Application where
  getLoc (Application l r) = getLoc l <> getLoc r

instance HasLoc InfixApplication where
  getLoc (InfixApplication l _ r) = getLoc l <> getLoc r

instance HasLoc PostfixApplication where
  getLoc (PostfixApplication l o) = getLoc l <> getLoc o

instance HasLoc (LambdaClause 'Scoped) where
  getLoc c =
    fmap getLoc (c ^. lambdaPipe . unIrrelevant)
      ?<> getLocSpan (c ^. lambdaParameters)
      <> getLoc (c ^. lambdaBody)

instance HasLoc (Lambda 'Scoped) where
  getLoc l = getLoc (l ^. lambdaKw) <> getLoc (l ^. lambdaBraces . unIrrelevant . _2)

instance HasLoc (FunctionParameter 'Scoped) where
  getLoc = \case
    FunctionParameterName n -> getLoc n
    FunctionParameterWildcard w -> getLoc w

instance HasLoc (FunctionParameters 'Scoped) where
  getLoc p = case p ^. paramDelims . unIrrelevant of
    Nothing -> (getLoc <$> listToMaybe (p ^. paramNames)) ?<> getLoc (p ^. paramType)
    Just (l, r) -> getLoc l <> getLoc r

instance HasLoc (Function 'Scoped) where
  getLoc f = getLoc (f ^. funParameters) <> getLoc (f ^. funReturn)

instance HasLoc (Let 'Scoped) where
  getLoc l = getLoc (l ^. letKw) <> getLoc (l ^. letExpression)

instance SingI s => HasLoc (CaseBranch s) where
  getLoc c = getLoc (c ^. caseBranchPipe) <> getLocExpressionType (c ^. caseBranchExpression)

instance SingI s => HasLoc (Case s) where
  getLoc c = getLoc (c ^. caseKw) <> getLoc (c ^. caseBranches . to last)

instance HasLoc (List s) where
  getLoc List {..} = getLoc _listBracketL <> getLoc _listBracketR

instance SingI s => HasLoc (NamedApplication s) where
  getLoc NamedApplication {..} = getLocIdentifierType _namedAppName <> getLoc (last _namedAppArgs)

instance HasLoc Expression where
  getLoc = \case
    ExpressionIdentifier i -> getLoc i
    ExpressionParensIdentifier i -> getLoc i
    ExpressionApplication i -> getLoc i
    ExpressionInfixApplication i -> getLoc i
    ExpressionPostfixApplication i -> getLoc i
    ExpressionLambda i -> getLoc i
    ExpressionList l -> getLoc l
    ExpressionCase i -> getLoc i
    ExpressionLet i -> getLoc i
    ExpressionUniverse i -> getLoc i
    ExpressionLiteral i -> getLoc i
    ExpressionFunction i -> getLoc i
    ExpressionHole i -> getLoc i
    ExpressionBraces i -> getLoc i
    ExpressionIterator i -> getLoc i
    ExpressionNamedApplication i -> getLoc i

getLocIdentifierType :: forall s. SingI s => IdentifierType s -> Interval
getLocIdentifierType e = case sing :: SStage s of
  SParsed -> getLoc e
  SScoped -> getLoc e

instance SingI s => HasLoc (Iterator s) where
  getLoc Iterator {..} = getLocIdentifierType _iteratorName <> getLocExpressionType _iteratorBody

instance SingI s => HasLoc (Import s) where
  getLoc Import {..} = case sing :: SStage s of
    SParsed -> getLoc _importModule
    SScoped -> getLoc _importModule

instance HasLoc (ModuleRef'' 'S.Concrete t) where
  getLoc ref = getLoc (ref ^. moduleRefName)

instance (SingI s, SingI t) => HasLoc (Module s t) where
  getLoc m = case sing :: SStage s of
    SParsed -> case sing :: SModuleIsTop t of
      SModuleLocal -> getLoc (m ^. modulePath)
      SModuleTop -> getLoc (m ^. modulePath)
    SScoped -> case sing :: SModuleIsTop t of
      SModuleLocal -> getLoc (m ^. modulePath)
      SModuleTop -> getLoc (m ^. modulePath)

getLocSymbolType :: forall s. SingI s => SymbolType s -> Interval
getLocSymbolType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

getLocExpressionType :: forall s. SingI s => ExpressionType s -> Interval
getLocExpressionType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

instance HasLoc (SigArg s) where
  getLoc SigArg {..} = getLoc l <> getLoc r
    where
      Irrelevant (l, r) = _sigArgDelims

instance SingI s => HasLoc (NewFunctionClause s) where
  getLoc NewFunctionClause {..} =
    getLoc _clausenPipeKw
      <> getLocExpressionType _clausenBody

instance SingI s => HasLoc (FunctionDefBody s) where
  getLoc = \case
    SigBodyExpression e -> getLocExpressionType e
    SigBodyClauses cl -> getLocSpan cl

instance SingI s => HasLoc (FunctionDef s) where
  getLoc FunctionDef {..} =
    (getLoc <$> _signDoc)
      ?<> (getLoc <$> _signPragmas)
      ?<> (getLoc <$> _signBuiltin)
      ?<> (getLoc <$> _signTerminating)
      ?<> getLocSymbolType _signName
      <> getLoc _signBody

instance SingI s => HasLoc (TypeSignature s) where
  getLoc TypeSignature {..} =
    (getLoc <$> _sigDoc)
      ?<> (getLoc <$> _sigPragmas)
      ?<> (getLoc <$> _sigBuiltin)
      ?<> (getLoc <$> _sigTerminating)
      ?<> getLocSymbolType _sigName
      <> (getLocExpressionType <$> _sigBody)
      ?<> getLocExpressionType _sigType

instance HasLoc (Example s) where
  getLoc e = e ^. exampleLoc

instance HasLoc (Judoc s) where
  getLoc (Judoc j) = getLocSpan j

instance HasLoc (JudocBlockParagraph s) where
  getLoc p = getLoc (p ^. judocBlockParagraphStart) <> getLoc (p ^. judocBlockParagraphEnd)

instance HasLoc (JudocGroup s) where
  getLoc = \case
    JudocGroupBlock l -> getLoc l
    JudocGroupLines l -> getLocSpan l

instance HasLoc (JudocBlock s) where
  getLoc = \case
    JudocLines ls -> getLocSpan ls
    JudocExample e -> getLoc e

instance HasLoc PatternScopedIden where
  getLoc = \case
    PatternScopedVar v -> getLoc v
    PatternScopedConstructor c -> getLoc c

instance HasLoc PatternBinding where
  getLoc (PatternBinding n p) = getLoc n <> getLoc p

instance HasLoc (ListPattern s) where
  getLoc l = getLoc (l ^. listpBracketL) <> getLoc (l ^. listpBracketR)

instance SingI s => HasLoc (PatternAtom s) where
  getLoc = \case
    PatternAtomIden i -> getLocIden i
    PatternAtomWildcard w -> getLoc w
    PatternAtomEmpty i -> i
    PatternAtomList l -> getLoc l
    PatternAtomParens p -> getLocParens p
    PatternAtomBraces p -> getLocParens p
    PatternAtomAt p -> getLocAt p
    where
      getLocAt :: forall r. (SingI r) => PatternAtType r -> Interval
      getLocAt p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p
      getLocIden :: forall r. (SingI r) => PatternAtomIdenType r -> Interval
      getLocIden p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p
      getLocParens :: forall r. (SingI r) => PatternParensType r -> Interval
      getLocParens p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p

instance HasLoc (JudocLine s) where
  getLoc (JudocLine delim atoms) = fmap getLoc delim ?<> getLocSpan atoms

instance HasLoc (PatternAtoms s) where
  getLoc = (^. patternAtomsLoc)

instance HasLoc PatternArg where
  getLoc a = fmap getLoc (a ^. patternArgName) ?<> getLoc (a ^. patternArgPattern)

instance HasLoc PatternInfixApp where
  getLoc (PatternInfixApp l _ r) =
    getLoc l <> getLoc r

instance HasLoc PatternPostfixApp where
  getLoc (PatternPostfixApp l _) = getLoc l

instance HasLoc PatternApp where
  getLoc (PatternApp l r) = getLoc l <> getLoc r

instance HasLoc Pattern where
  getLoc = \case
    PatternVariable v -> getLoc v
    PatternConstructor c -> getLoc c
    PatternApplication a -> getLoc a
    PatternWildcard w -> getLoc w
    PatternList w -> getLoc w
    PatternEmpty i -> i
    PatternInfixApplication i -> getLoc i
    PatternPostfixApplication i -> getLoc i

instance (Eq (PatternAtom s)) => Eq (PatternAtoms s) where
  (==) = (==) `on` (^. patternAtoms)

instance (Ord (PatternAtom s)) => Ord (PatternAtoms s) where
  compare = compare `on` (^. patternAtoms)

instance HasLoc (ExpressionAtoms s) where
  getLoc = (^. expressionAtomsLoc)

instance HasAtomicity (ExpressionAtoms 'Parsed) where
  atomicity ExpressionAtoms {..} = case _expressionAtoms of
    (_ :| []) -> Atom
    (_ :| _)
      | any isArrow _expressionAtoms -> Aggregate funFixity
      | otherwise -> Aggregate appFixity
      where
        isArrow :: ExpressionAtom s -> Bool
        isArrow = \case
          AtomFunArrow {} -> True
          _ -> False

instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (HoleType s),
    Eq (AmbiguousIteratorType s),
    Eq (SymbolType s),
    Eq (PatternParensType s),
    Eq (PatternAtomType s)
  ) =>
  Eq (ExpressionAtoms s)
  where
  (==) = (==) `on` (^. expressionAtoms)

instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (AmbiguousIteratorType s),
    Ord (HoleType s),
    Ord (SymbolType s),
    Ord (PatternParensType s),
    Ord (PatternAtomType s)
  ) =>
  Ord (ExpressionAtoms s)
  where
  compare = compare `on` (^. expressionAtoms)

data ApeLeaf
  = ApeLeafExpression Expression
  | ApeLeafFunctionParams (FunctionParameters 'Scoped)
  | ApeLeafArgumentBlock (AnyStage ArgumentBlock)
  | ApeLeafFunctionKw KeywordRef
  | ApeLeafPattern Pattern
  | ApeLeafPatternArg PatternArg
  | ApeLeafAtom (AnyStage ExpressionAtom)

instance IsApe PatternApp ApeLeaf where
  toApe (PatternApp l r) =
    ApeApp
      Ape.App
        { _appLeft = toApe l,
          _appRight = toApe r
        }

instance IsApe Pattern ApeLeaf where
  toApe = \case
    PatternApplication a -> toApe a
    PatternInfixApplication a -> toApe a
    PatternPostfixApplication a -> toApe a
    e ->
      ApeLeaf
        ( Leaf
            { _leafAtomicity = atomicity e,
              _leafExpr = ApeLeafPattern e
            }
        )

instance IsApe PatternArg ApeLeaf where
  toApe pa
    | Atom == atomicity pa =
        ApeLeaf
          ( Leaf
              { _leafAtomicity = Atom,
                _leafExpr = ApeLeafPatternArg pa
              }
          )
    | otherwise = toApe (pa ^. patternArgPattern)

instance IsApe PatternPostfixApp ApeLeaf where
  toApe p@(PatternPostfixApp l op) =
    ApePostfix
      Postfix
        { _postfixFixity = getFixity p,
          _postfixLeft = toApe l,
          _postfixOp = ApeLeafPattern (PatternConstructor op)
        }

instance IsApe PatternInfixApp ApeLeaf where
  toApe i@(PatternInfixApp l op r) =
    ApeInfix
      Infix
        { _infixFixity = getFixity i,
          _infixLeft = toApe l,
          _infixRight = toApe r,
          _infixIsDelimiter = isDelimiterStr (prettyText (op ^. S.nameConcrete)),
          _infixOp = ApeLeafPattern (PatternConstructor op)
        }

instance IsApe ScopedIden ApeLeaf where
  toApe iden =
    ApeLeaf
      ( Leaf
          { _leafAtomicity = Atom,
            _leafExpr = ApeLeafExpression (ExpressionIdentifier iden)
          }
      )

instance SingI s => IsApe (ArgumentBlock s) ApeLeaf where
  toApe b =
    ApeLeaf
      ( Leaf
          { _leafAtomicity = atomicity b,
            _leafExpr = ApeLeafArgumentBlock (sing :&: b)
          }
      )

toApeIdentifierType :: forall s. SingI s => IdentifierType s -> Ape ApeLeaf
toApeIdentifierType = case sing :: SStage s of
  SParsed -> toApe
  SScoped -> toApe

instance IsApe Name ApeLeaf where
  toApe n =
    ApeLeaf
      ( Leaf
          { _leafAtomicity = atomicity n,
            _leafExpr = ApeLeafAtom (sing :&: AtomIdentifier n)
          }
      )

instance SingI s => IsApe (NamedApplication s) ApeLeaf where
  toApe NamedApplication {..} = mkApps f (toApe <$> _namedAppArgs)
    where
      f = toApeIdentifierType _namedAppName

instance IsApe Application ApeLeaf where
  toApe (Application l r) =
    ApeApp
      Ape.App
        { _appLeft = toApe l,
          _appRight = toApe r
        }

instance IsApe InfixApplication ApeLeaf where
  toApe i@(InfixApplication l op r) =
    ApeInfix
      Infix
        { _infixFixity = getFixity i,
          _infixLeft = toApe l,
          _infixRight = toApe r,
          _infixIsDelimiter = isDelimiterStr (prettyText (identifierName op ^. S.nameConcrete)),
          _infixOp = ApeLeafExpression (ExpressionIdentifier op)
        }

instance IsApe PostfixApplication ApeLeaf where
  toApe p@(PostfixApplication l op) =
    ApePostfix
      Postfix
        { _postfixFixity = getFixity p,
          _postfixLeft = toApe l,
          _postfixOp = ApeLeafExpression (ExpressionIdentifier op)
        }

instance IsApe (Function 'Scoped) ApeLeaf where
  toApe (Function ps kw ret) =
    ApeInfix
      Infix
        { _infixFixity = funFixity,
          _infixLeft = toApe ps,
          _infixRight = toApe ret,
          _infixIsDelimiter = False,
          _infixOp = ApeLeafFunctionKw kw
        }

instance IsApe Expression ApeLeaf where
  toApe e = case e of
    ExpressionApplication a -> toApe a
    ExpressionInfixApplication a -> toApe a
    ExpressionPostfixApplication a -> toApe a
    ExpressionFunction a -> toApe a
    ExpressionNamedApplication a -> toApe a
    ExpressionParensIdentifier {} -> leaf
    ExpressionIdentifier {} -> leaf
    ExpressionList {} -> leaf
    ExpressionCase {} -> leaf
    ExpressionLambda {} -> leaf
    ExpressionLet {} -> leaf
    ExpressionUniverse {} -> leaf
    ExpressionHole {} -> leaf
    ExpressionLiteral {} -> leaf
    ExpressionBraces {} -> leaf
    ExpressionIterator {} -> leaf
    where
      leaf =
        ApeLeaf
          ( Leaf
              { _leafAtomicity = atomicity e,
                _leafExpr = ApeLeafExpression e
              }
          )

instance IsApe (FunctionParameters 'Scoped) ApeLeaf where
  toApe f
    | atomicity f == Atom =
        ApeLeaf
          ( Leaf
              { _leafAtomicity = Atom,
                _leafExpr = ApeLeafFunctionParams f
              }
          )
    | otherwise = toApe (f ^. paramType)

instance HasAtomicity PatternArg where
  atomicity p
    | Implicit <- p ^. patternArgIsImplicit = Atom
    | isJust (p ^. patternArgName) = Atom
    | otherwise = atomicity (p ^. patternArgPattern)

idenOverName :: (forall s. S.Name' s -> S.Name' s) -> ScopedIden -> ScopedIden
idenOverName f = \case
  ScopedAxiom a -> ScopedAxiom (f a)
  ScopedInductive i -> ScopedInductive (f i)
  ScopedVar v -> ScopedVar (f v)
  ScopedFunction fun -> ScopedFunction (f fun)
  ScopedConstructor c -> ScopedConstructor (f c)

entryPrism :: (S.Name' () -> S.Name' ()) -> SymbolEntry -> (S.Name' (), SymbolEntry)
entryPrism f = \case
  EntryAxiom a -> (a, EntryAxiom (f a))
  EntryInductive i -> (i, EntryInductive (f i))
  EntryFunction fun -> (fun, EntryFunction (f fun))
  EntryConstructor c -> (c, EntryConstructor (f c))
  EntryModule m -> (getModuleRefNameType m, EntryModule (overModuleRef'' (over moduleRefName f) m))
  EntryVariable m -> (m, EntryVariable (f m))

entryOverName :: (S.Name' () -> S.Name' ()) -> SymbolEntry -> SymbolEntry
entryOverName f = snd . entryPrism f

entryName :: SymbolEntry -> S.Name' ()
entryName = fst . entryPrism id

entryIsExpression :: SymbolEntry -> Bool
entryIsExpression = \case
  EntryAxiom {} -> True
  EntryInductive {} -> True
  EntryFunction {} -> True
  EntryConstructor {} -> True
  EntryVariable {} -> True
  EntryModule {} -> False

judocExamples :: Judoc s -> [Example s]
judocExamples (Judoc bs) = concatMap goGroup bs
  where
    goGroup :: JudocGroup s -> [Example s]
    goGroup = \case
      JudocGroupBlock p -> goParagraph p
      JudocGroupLines l -> concatMap goBlock l

    goParagraph :: JudocBlockParagraph s -> [Example s]
    goParagraph l = concatMap goBlock (l ^. judocBlockParagraphBlocks)

    goBlock :: JudocBlock s -> [Example s]
    goBlock = \case
      JudocExample e -> [e]
      _ -> mempty

instance HasLoc SymbolEntry where
  getLoc = (^. S.nameDefined) . entryName

overModuleRef'' :: forall s s'. (forall t. ModuleRef'' s t -> ModuleRef'' s' t) -> ModuleRef' s -> ModuleRef' s'
overModuleRef'' f = over unModuleRef' (\(t :&: m'') -> t :&: f m'')

symbolEntryNameId :: SymbolEntry -> NameId
symbolEntryNameId = (^. S.nameId) . symbolEntryToSName

symbolEntryToSName :: SymbolEntry -> S.Name' ()
symbolEntryToSName = \case
  EntryAxiom a -> a
  EntryInductive i -> i
  EntryFunction f -> f
  EntryConstructor c -> c
  EntryModule m -> getModuleRefNameType m
  EntryVariable m -> m

instance HasNameKind ScopedIden where
  getNameKind = \case
    ScopedAxiom {} -> KNameAxiom
    ScopedInductive {} -> KNameInductive
    ScopedConstructor {} -> KNameConstructor
    ScopedVar {} -> KNameLocal
    ScopedFunction {} -> KNameFunction

instance HasNameKind SymbolEntry where
  getNameKind = getNameKind . entryName
