{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Concrete.Language
  ( module Juvix.Compiler.Concrete.Language,
    module Juvix.Compiler.Concrete.Data.Name,
    module Juvix.Compiler.Concrete.Data.NameRef,
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
import Juvix.Compiler.Concrete.Data.PublicAnn
import Juvix.Compiler.Concrete.Data.ScopedName (unqualifiedSymbol)
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Data
import Juvix.Data.Ape.Base as Ape
import Juvix.Data.Fixity
import Juvix.Data.NameKind
import Juvix.Prelude hiding (show)
import Prelude (show)

data Stage
  = Parsed
  | Scoped
  deriving stock (Eq, Show)

$(genSingletons [''Stage])

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
  HoleType 'Parsed = Interval
  HoleType 'Scoped = Hole

type PatternAtomIdenType :: Stage -> GHC.Type
type family PatternAtomIdenType s = res | res -> s where
  PatternAtomIdenType 'Parsed = Name
  PatternAtomIdenType 'Scoped = PatternScopedIden

type ExpressionType :: Stage -> GHC.Type
type family ExpressionType s = res | res -> s where
  ExpressionType 'Parsed = ExpressionAtoms 'Parsed
  ExpressionType 'Scoped = Expression

type PatternType :: Stage -> GHC.Type
type family PatternType s = res | res -> s where
  PatternType 'Parsed = PatternAtom 'Parsed
  PatternType 'Scoped = PatternArg

type PatternParensType :: Stage -> GHC.Type
type family PatternParensType s = res | res -> s where
  PatternParensType 'Parsed = PatternAtoms 'Parsed
  PatternParensType 'Scoped = PatternArg

type PatternAtType :: Stage -> GHC.Type
type family PatternAtType s = res | res -> s where
  PatternAtType 'Parsed = PatternBinding
  PatternAtType 'Scoped = PatternArg

type family ImportType (s :: Stage) :: GHC.Type where
  ImportType 'Parsed = TopModulePath
  ImportType 'Scoped = Module 'Scoped 'ModuleTop

type ModulePathType :: Stage -> ModuleIsTop -> GHC.Type
type family ModulePathType s t = res | res -> t s where
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
  | StatementForeign ForeignBlock
  | StatementCompile (Compile s)

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

--------------------------------------------------------------------------------
-- Import statement
--------------------------------------------------------------------------------

newtype Import (s :: Stage) = Import
  { _importModule :: ImportType s
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
  { _opSymbol :: Symbol,
    _opFixity :: Fixity
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc OperatorSyntaxDef where
  getLoc OperatorSyntaxDef {..} = getLoc _opSymbol

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature (s :: Stage) = TypeSignature
  { _sigName :: FunctionName s,
    _sigType :: ExpressionType s,
    _sigDoc :: Maybe (Judoc s),
    _sigBuiltin :: Maybe BuiltinFunction,
    _sigTerminating :: Bool
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (TypeSignature s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (TypeSignature s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (TypeSignature s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef (s :: Stage) = AxiomDef
  { _axiomDoc :: Maybe (Judoc s),
    _axiomName :: SymbolType s,
    _axiomBuiltin :: Maybe BuiltinAxiom,
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
  { _constructorName :: InductiveConstructorName s,
    _constructorDoc :: Maybe (Judoc s),
    _constructorType :: ExpressionType s
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
  { _inductiveBuiltin :: Maybe BuiltinInductive,
    _inductiveDoc :: Maybe (Judoc s),
    _inductiveName :: InductiveName s,
    _inductiveParameters :: [InductiveParameter s],
    _inductiveType :: Maybe (ExpressionType s),
    _inductiveConstructors :: [InductiveConstructorDef s],
    _inductivePositive :: Bool
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
    _patInfixConstructor :: ConstructorRef,
    _patInfixRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PatternInfixApp where
  getFixity (PatternInfixApp _ op _) = fromMaybe impossible (op ^. constructorRefName . S.nameFixity)

data PatternPostfixApp = PatternPostfixApp
  { _patPostfixParameter :: PatternArg,
    _patPostfixConstructor :: ConstructorRef
  }
  deriving stock (Show, Eq, Ord)

instance HasFixity PatternPostfixApp where
  getFixity (PatternPostfixApp _ op) = fromMaybe impossible (op ^. constructorRefName . S.nameFixity)

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgName :: Maybe S.Symbol,
    _patternArgPattern :: Pattern
  }
  deriving stock (Show, Eq, Ord)

data Pattern
  = PatternVariable (SymbolType 'Scoped)
  | PatternConstructor ConstructorRef
  | PatternApplication PatternApp
  | PatternInfixApplication PatternInfixApp
  | PatternPostfixApplication PatternPostfixApp
  | PatternWildcard Wildcard
  | PatternEmpty Interval
  deriving stock (Show, Eq, Ord)

instance HasAtomicity Pattern where
  atomicity e = case e of
    PatternVariable {} -> Atom
    PatternConstructor {} -> Atom
    PatternApplication {} -> Aggregate appFixity
    PatternInfixApplication a -> Aggregate (getFixity a)
    PatternPostfixApplication p -> Aggregate (getFixity p)
    PatternWildcard {} -> Atom
    PatternEmpty {} -> Atom

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternScopedIden
  = PatternScopedVar S.Symbol
  | PatternScopedConstructor ConstructorRef
  deriving stock (Show, Ord, Eq)

data PatternBinding = PatternBinding
  { _patternBindingName :: Symbol,
    _patternBindingPattern :: PatternAtom 'Parsed
  }

data PatternAtom (s :: Stage)
  = PatternAtomIden (PatternAtomIdenType s)
  | PatternAtomWildcard Wildcard
  | PatternAtomEmpty Interval
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
    _clausePatterns :: [PatternType s],
    _clauseBody :: ExpressionType s
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
    _moduleDoc :: Maybe (Judoc s),
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
  S.SConcrete -> (^. S.nameId)
  S.SNotConcrete -> (^. S.nameId)

getModuleExportInfo :: ModuleRef' c -> ExportInfo
getModuleExportInfo (ModuleRef' (_ :&: ModuleRef'' {..})) = _moduleExportInfo

getModuleRefNameType :: ModuleRef' c -> RefNameType c
getModuleRefNameType (ModuleRef' (_ :&: ModuleRef'' {..})) = _moduleRefName

instance SingI c => Eq (ModuleRef' c) where
  (==) = (==) `on` (getNameRefId . getModuleRefNameType)

instance SingI c => Ord (ModuleRef' c) where
  compare = compare `on` (getNameRefId . getModuleRefNameType)

data ModuleRef'' (c :: S.IsConcrete) (t :: ModuleIsTop) = ModuleRef''
  { _moduleRefName :: RefNameType c,
    _moduleExportInfo :: ExportInfo,
    _moduleRefModule :: Module 'Scoped t
  }

instance Show (RefNameType s) => Show (ModuleRef'' s t) where
  show ModuleRef'' {..} = show _moduleRefName

data SymbolEntry
  = EntryAxiom (AxiomRef' 'S.NotConcrete)
  | EntryInductive (InductiveRef' 'S.NotConcrete)
  | EntryFunction (FunctionRef' 'S.NotConcrete)
  | EntryConstructor (ConstructorRef' 'S.NotConcrete)
  | EntryModule (ModuleRef' 'S.NotConcrete)
  deriving stock (Show)

-- | Symbols that a module exports
newtype ExportInfo = ExportInfo
  { _exportSymbols :: HashMap Symbol SymbolEntry
  }
  deriving stock (Show)

data OpenModule (s :: Stage) = OpenModule
  { _openModuleName :: ModuleRefType s,
    _openModuleImport :: Bool,
    _openParameters :: [ExpressionType s],
    _openUsingHiding :: Maybe UsingHiding,
    _openPublic :: PublicAnn
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
  ScopedAxiom a -> a ^. axiomRefName
  ScopedInductive i -> i ^. inductiveRefName
  ScopedVar v ->
    ( case sing :: S.SIsConcrete n of
        S.SConcrete -> id
        S.SNotConcrete -> set S.nameConcrete ()
    )
      (unqualifiedSymbol v)
  ScopedFunction f -> f ^. functionRefName
  ScopedConstructor c -> c ^. constructorRefName

data Expression
  = ExpressionIdentifier ScopedIden
  | ExpressionParensIdentifier ScopedIden
  | ExpressionApplication Application
  | ExpressionInfixApplication InfixApplication
  | ExpressionPostfixApplication PostfixApplication
  | ExpressionLambda (Lambda 'Scoped)
  | ExpressionLetBlock (LetBlock 'Scoped)
  | ExpressionUniverse Universe
  | ExpressionLiteral LiteralLoc
  | ExpressionFunction (Function 'Scoped)
  | ExpressionHole (HoleType 'Scoped)
  | ExpressionBraces (WithLoc Expression)
  deriving stock (Show, Eq, Ord)

instance HasAtomicity Expression where
  atomicity e = case e of
    ExpressionIdentifier {} -> Atom
    ExpressionHole {} -> Atom
    ExpressionParensIdentifier {} -> Atom
    ExpressionApplication {} -> Aggregate appFixity
    ExpressionInfixApplication a -> Aggregate (getFixity a)
    ExpressionPostfixApplication a -> Aggregate (getFixity a)
    ExpressionLambda {} -> Atom
    ExpressionLiteral {} -> Atom
    ExpressionLetBlock {} -> Atom
    ExpressionBraces {} -> Atom
    ExpressionUniverse {} -> Atom
    ExpressionFunction {} -> Aggregate funFixity

--------------------------------------------------------------------------------
-- Function expression
--------------------------------------------------------------------------------

data FunctionParameter (s :: Stage) = FunctionParameter
  { _paramName :: Maybe (SymbolType s),
    _paramUsage :: Maybe Usage,
    _paramImplicit :: IsImplicit,
    _paramType :: ExpressionType s
  }

deriving stock instance
  (Show (ExpressionType s), Show (SymbolType s)) =>
  Show (FunctionParameter s)

deriving stock instance
  (Eq (ExpressionType s), Eq (SymbolType s)) =>
  Eq (FunctionParameter s)

deriving stock instance
  (Ord (ExpressionType s), Ord (SymbolType s)) =>
  Ord (FunctionParameter s)

data Function (s :: Stage) = Function
  { _funParameter :: FunctionParameter s,
    _funReturn :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (Function s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (Function s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (Function s)

--------------------------------------------------------------------------------
-- Lambda expression
--------------------------------------------------------------------------------

-- Notes: An empty lambda, here called 'the impossible case', is a lambda
-- expression with empty list of arguments and empty body.

newtype Lambda (s :: Stage) = Lambda
  { _lambdaClauses :: [LambdaClause s]
  }

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
  { _lambdaParameters :: NonEmpty (PatternType s),
    _lambdaBody :: ExpressionType s
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

data LetBlock (s :: Stage) = LetBlock
  { _letClauses :: [LetClause s],
    _letExpression :: ExpressionType s
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
-- Compile statements
--------------------------------------------------------------------------------

data Compile s = Compile
  { _compileName :: SymbolType s,
    _compileBackendItems :: [BackendItem]
  }

deriving stock instance
  (Show (SymbolType s)) => Show (Compile s)

deriving stock instance
  (Ord (SymbolType s)) => Ord (Compile s)

deriving stock instance
  (Eq (SymbolType s)) => Eq (Compile s)

--------------------------------------------------------------------------------
-- Debugging statements
--------------------------------------------------------------------------------

newtype Eval (s :: Stage) = Eval {evalExpression :: ExpressionType s}

deriving stock instance
  Show (ExpressionType s) => Show (Eval s)

deriving stock instance
  Eq (ExpressionType s) => Eq (Eval s)

deriving stock instance
  Ord (ExpressionType s) => Ord (Eval s)

--------------------------------------------------------------------------------

newtype Print (s :: Stage) = Print {printExpression :: ExpressionType s}

deriving stock instance
  Show (ExpressionType s) => Show (Print s)

deriving stock instance
  Eq (ExpressionType s) => Eq (Print s)

deriving stock instance
  Ord (ExpressionType s) => Ord (Print s)

--------------------------------------------------------------------------------
-- Expression atom
--------------------------------------------------------------------------------

-- | Expressions without application
data ExpressionAtom (s :: Stage)
  = AtomIdentifier (IdentifierType s)
  | AtomLambda (Lambda s)
  | AtomHole (HoleType s)
  | AtomBraces (WithLoc (ExpressionType s))
  | AtomLetBlock (LetBlock s)
  | AtomUniverse Universe
  | AtomFunction (Function s)
  | AtomFunArrow
  | AtomLiteral LiteralLoc
  | AtomParens (ExpressionType s)

data ExpressionAtoms (s :: Stage) = ExpressionAtoms
  { _expressionAtoms :: NonEmpty (ExpressionAtom s),
    _expressionAtomsLoc :: Interval
  }

newtype Judoc (s :: Stage) = Judoc
  { _block :: [JudocBlock s]
  }
  deriving newtype (Semigroup, Monoid)

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (Judoc s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (Judoc s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (Judoc s)

data Example (s :: Stage) = Example
  { _exampleId :: NameId,
    _exampleExpression :: ExpressionType s
  }

deriving stock instance Show (ExpressionType s) => Show (Example s)

deriving stock instance Eq (ExpressionType s) => Eq (Example s)

deriving stock instance Ord (ExpressionType s) => Ord (Example s)

data JudocBlock (s :: Stage)
  = JudocParagraph (NonEmpty (JudocParagraphLine s))
  | JudocExample (Example s)

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocBlock s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocBlock s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocBlock s)

newtype JudocParagraphLine (s :: Stage)
  = JudocParagraphLine (NonEmpty (JudocAtom s))

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocParagraphLine s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocParagraphLine s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocParagraphLine s)

data JudocAtom (s :: Stage)
  = JudocExpression (ExpressionType s)
  | JudocText Text

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (JudocAtom s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (JudocAtom s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (JudocAtom s)

makeLenses ''PatternArg
makeLenses ''Example
makeLenses ''Judoc
makeLenses ''Function
makeLenses ''InductiveDef
makeLenses ''PostfixApplication
makeLenses ''InfixApplication
makeLenses ''Application
makeLenses ''LetBlock
makeLenses ''FunctionParameter
makeLenses ''Import
makeLenses ''OperatorSyntaxDef
makeLenses ''InductiveConstructorDef
makeLenses ''Module
makeLenses ''TypeSignature
makeLenses ''AxiomDef
makeLenses ''FunctionClause
makeLenses ''InductiveParameter
makeLenses ''ModuleRef'
makeLenses ''ModuleRef''
makeLenses ''OpenModule
makeLenses ''PatternApp
makeLenses ''PatternInfixApp
makeLenses ''PatternPostfixApp
makeLenses ''Compile
makeLenses ''PatternBinding
makeLenses ''PatternAtoms
makeLenses ''ExpressionAtoms

instance HasAtomicity (PatternAtom 'Parsed) where
  atomicity = const Atom

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
    Show (PatternAtomIdenType s),
    Show (PatternParensType s),
    Show (PatternAtType s),
    Show (PatternType s)
  ) =>
  Show (PatternAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (PatternAtomIdenType s),
    Eq (PatternParensType s),
    Eq (PatternAtType s),
    Eq (PatternType s)
  ) =>
  Eq (PatternAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (PatternAtomIdenType s),
    Ord (PatternParensType s),
    Ord (PatternAtType s),
    Ord (PatternType s)
  ) =>
  Ord (PatternAtom s)

deriving stock instance
  Show (PatternAtom s) => Show (PatternAtoms s)

instance HasLoc PatternScopedIden where
  getLoc = \case
    PatternScopedVar v -> getLoc v
    PatternScopedConstructor c -> getLoc c

instance HasLoc PatternBinding where
  getLoc (PatternBinding n p) = getLoc n <> getLoc p

instance SingI s => HasLoc (PatternAtom s) where
  getLoc = \case
    PatternAtomIden i -> getLocIden i
    PatternAtomWildcard w -> getLoc w
    PatternAtomEmpty i -> i
    PatternAtomParens p -> getLocParens p
    PatternAtomBraces p -> getLocParens p
    PatternAtomAt p -> getLocAt p
    where
      getLocAt :: forall r. SingI r => PatternAtType r -> Interval
      getLocAt p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p
      getLocIden :: forall r. SingI r => PatternAtomIdenType r -> Interval
      getLocIden p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p
      getLocParens :: forall r. SingI r => PatternParensType r -> Interval
      getLocParens p = case sing :: SStage r of
        SParsed -> getLoc p
        SScoped -> getLoc p

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
    PatternEmpty i -> i
    PatternInfixApplication i -> getLoc i
    PatternPostfixApplication i -> getLoc i

instance Eq (PatternAtom s) => Eq (PatternAtoms s) where
  (==) = (==) `on` (^. patternAtoms)

instance Ord (PatternAtom s) => Ord (PatternAtoms s) where
  compare = compare `on` (^. patternAtoms)

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (HoleType s),
    Show (SymbolType s),
    Show (PatternType s)
  ) =>
  Show (ExpressionAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (HoleType s),
    Eq (ModuleRefType s),
    Eq (SymbolType s),
    Eq (PatternType s)
  ) =>
  Eq (ExpressionAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (HoleType s),
    Ord (SymbolType s),
    Ord (PatternType s)
  ) =>
  Ord (ExpressionAtom s)

deriving stock instance
  ( Show (ExpressionType s),
    Show (IdentifierType s),
    Show (ModuleRefType s),
    Show (HoleType s),
    Show (SymbolType s),
    Show (PatternType s)
  ) =>
  Show (ExpressionAtoms s)

instance HasLoc (ExpressionAtoms s) where
  getLoc = (^. expressionAtomsLoc)

instance HasAtomicity (ExpressionAtoms 'Parsed) where
  atomicity ExpressionAtoms {..} = case _expressionAtoms of
    (_ :| []) -> Atom
    (_ :| _)
      | AtomFunArrow `elem` _expressionAtoms -> Aggregate funFixity
      | otherwise -> Aggregate appFixity

instance
  ( Eq (ExpressionType s),
    Eq (IdentifierType s),
    Eq (ModuleRefType s),
    Eq (HoleType s),
    Eq (SymbolType s),
    Eq (PatternType s)
  ) =>
  Eq (ExpressionAtoms s)
  where
  (==) = (==) `on` (^. expressionAtoms)

instance
  ( Ord (ExpressionType s),
    Ord (IdentifierType s),
    Ord (ModuleRefType s),
    Ord (HoleType s),
    Ord (SymbolType s),
    Ord (PatternType s)
  ) =>
  Ord (ExpressionAtoms s)
  where
  compare = compare `on` (^. expressionAtoms)

--------------------------------------------------------------------------------

instance IsApe Application Expression where
  toApe (Application l r) =
    ApeApp
      Ape.App
        { _appLeft = toApe l,
          _appRight = toApe r
        }

instance IsApe InfixApplication Expression where
  toApe i@(InfixApplication l op r) =
    ApeInfix
      Infix
        { _infixFixity = getFixity i,
          _infixLeft = toApe l,
          _infixRight = toApe r,
          _infixOp = ExpressionIdentifier op
        }

instance IsApe PostfixApplication Expression where
  toApe p@(PostfixApplication l op) =
    ApePostfix
      Postfix
        { _postfixFixity = getFixity p,
          _postfixLeft = toApe l,
          _postfixOp = ExpressionIdentifier op
        }

instance IsApe Expression Expression where
  toApe = \case
    ExpressionApplication a -> toApe a
    ExpressionInfixApplication a -> toApe a
    ExpressionPostfixApplication a -> toApe a
    e ->
      ApeLeaf
        ( Leaf
            { _leafAtomicity = atomicity e,
              _leafExpr = e
            }
        )

instance HasAtomicity PatternArg where
  atomicity p
    | Implicit <- p ^. patternArgIsImplicit = Atom
    | isJust (p ^. patternArgName) = Atom
    | otherwise = atomicity (p ^. patternArgPattern)

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

entryIsExpression :: SymbolEntry -> Bool
entryIsExpression = \case
  EntryAxiom {} -> True
  EntryInductive {} -> True
  EntryFunction {} -> True
  EntryConstructor {} -> True
  EntryModule {} -> False

judocExamples :: Judoc s -> [Example s]
judocExamples (Judoc bs) = concatMap go bs
  where
    go :: JudocBlock s -> [Example s]
    go = \case
      JudocExample e -> [e]
      _ -> mempty

instance HasLoc SymbolEntry where
  getLoc = (^. S.nameDefined) . entryName

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
