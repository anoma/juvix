
{-# LANGUAGE UndecidableInstances #-}

module MiniJuvix.Syntax.Concrete.Language
  ( module MiniJuvix.Syntax.Concrete.Language,
    module MiniJuvix.Syntax.Concrete.Name,
    module MiniJuvix.Syntax.Concrete.Loc,
    module MiniJuvix.Syntax.Concrete.PublicAnn,
    module MiniJuvix.Syntax.Concrete.Language.Stage,
    module MiniJuvix.Syntax.Concrete.Fixity,
  )
where

--------------------------------------------------------------------------------

import qualified Data.Kind as GHC
import Data.Singletons
import MiniJuvix.Syntax.Concrete.Fixity
import MiniJuvix.Syntax.Concrete.Name
import MiniJuvix.Syntax.Concrete.Loc
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import MiniJuvix.Syntax.Concrete.PublicAnn
import MiniJuvix.Syntax.Concrete.Language.Stage
import MiniJuvix.Prelude

--------------------------------------------------------------------------------
-- Parsing stages
--------------------------------------------------------------------------------

type family SymbolType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  SymbolType 'Parsed = Symbol
  SymbolType 'Scoped = S.Symbol

type family NameType (s :: Stage) :: (res :: GHC.Type) | res -> s where
  NameType 'Parsed = Name
  NameType 'Scoped = S.Name

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

deriving stock instance
  ( Show (ImportType s),
    Show (ModulePathType s 'ModuleLocal),
    Show (PatternType s),
    Show (SymbolType s),
    Show (NameType s),
    Show (ExpressionType s)
  ) =>
  Show (Statement s)

deriving stock instance
  ( Eq (ImportType s),
    Eq (PatternType s),
    Eq (ModulePathType s 'ModuleLocal),
    Eq (SymbolType s),
    Eq (NameType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Statement s)

deriving stock instance
  ( Ord (ImportType s),
    Ord (PatternType s),
    Ord (ModulePathType s 'ModuleLocal),
    Ord (SymbolType s),
    Ord (NameType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Statement s)

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
-- Quantity
-------------------------------------------------------------------------------

data Usage
  = UsageNone
  | UsageOnce
  | UsageOmega
  deriving stock (Show, Eq, Ord, Data)

-------------------------------------------------------------------------------
-- Type signature declaration
-------------------------------------------------------------------------------

data TypeSignature (s :: Stage) = TypeSignature
  { sigName :: FunctionName s,
    sigType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (TypeSignature s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (TypeSignature s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (TypeSignature s)

deriving stock instance (Data (ExpressionType s), Data (SymbolType s), Typeable s) => Data (TypeSignature s)

-------------------------------------------------------------------------------
-- Axioms
-------------------------------------------------------------------------------

data AxiomDef (s :: Stage) = AxiomDef
  { axiomName :: SymbolType s,
    axiomType :: ExpressionType s
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
  { inductiveParameterName :: SymbolType s,
    inductiveParameterType :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveParameter s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveParameter s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveParameter s)

data InductiveDef (s :: Stage) = InductiveDef
  { inductiveName :: InductiveName s,
    inductiveParameters :: [InductiveParameter s],
    inductiveType :: Maybe (ExpressionType s),
    inductiveConstructors :: [InductiveConstructorDef s]
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (InductiveDef s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (InductiveDef s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (InductiveDef s)

--------------------------------------------------------------------------------
-- Pattern
--------------------------------------------------------------------------------

data PatternInfixApp = PatternInfixApp
  {
    patInfixLeft :: Pattern,
    patInfixConstructor :: NameType 'Scoped,
    patInfixRight :: Pattern
  }
  deriving stock (Show, Eq, Ord, Data)

data PatternPostfixApp = PatternPostfixApp
  {
    patPostfixParameter :: Pattern,
    patPostfixConstructor :: NameType 'Scoped
  }
  deriving stock (Show, Eq, Ord, Data)

data Pattern
  = PatternVariable (SymbolType 'Scoped)
  | PatternConstructor (NameType 'Scoped)
  | PatternApplication Pattern Pattern
  | PatternInfixApplication PatternInfixApp
  | PatternPostfixApplication PatternPostfixApp
  | PatternWildcard
  | PatternEmpty
  deriving stock (Show, Eq, Ord, Data)

--------------------------------------------------------------------------------
-- Pattern section
--------------------------------------------------------------------------------

data PatternAtom (s :: Stage)
  = PatternAtomName (NameType s)
  | PatternAtomWildcard
  | PatternAtomEmpty
  | PatternAtomParens (PatternAtoms s)

deriving stock instance
  ( Show (ExpressionType s),
    Show (NameType s),
    Show (PatternType s)
  ) =>
  Show (PatternAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (NameType s),
    Eq (PatternType s)
  ) =>
  Eq (PatternAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (NameType s),
    Ord (PatternType s)
  ) =>
  Ord (PatternAtom s)

deriving stock instance
  ( Data (ExpressionType s),
    Data (NameType s),
    Data (PatternType s),
    Typeable s
  ) =>
  Data (PatternAtom s)

newtype PatternAtoms (s :: Stage)
  = PatternAtoms (NonEmpty (PatternAtom s))

deriving stock instance
  ( Show (ExpressionType s),
    Show (NameType s),
    Show (PatternType s)
  ) =>
  Show (PatternAtoms s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (NameType s),
    Eq (PatternType s)
  ) =>
  Eq (PatternAtoms s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (NameType s),
    Ord (PatternType s)
  ) =>
  Ord (PatternAtoms s)

deriving stock instance
  ( Data (ExpressionType s),
    Data (NameType s),
    Data (PatternType s),
    Typeable s
  ) =>
  Data (PatternAtoms s)

--------------------------------------------------------------------------------
-- Function binding declaration
--------------------------------------------------------------------------------

type FunctionName s = SymbolType s

data FunctionClause (s :: Stage) = FunctionClause
  { clauseOwnerFunction :: FunctionName s,
    clausePatterns :: [PatternType s],
    clauseBody :: ExpressionType s,
    clauseWhere :: Maybe (WhereBlock s)
  }

deriving stock instance
  ( Show (PatternType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (FunctionClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (FunctionClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (FunctionClause s)

deriving stock instance
  ( Data (PatternType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (FunctionClause s)

--------------------------------------------------------------------------------
-- Module declaration
--------------------------------------------------------------------------------

data ModuleIsTop = ModuleTop | ModuleLocal

-- The following Singleton related definitions could be scrapped if we depended
-- on the singletons-th library.
data SModuleIsTop (t :: ModuleIsTop) where
  SModuleTop :: SModuleIsTop 'ModuleTop
  SModuleLocal :: SModuleIsTop 'ModuleLocal

type instance Sing = SModuleIsTop

instance SingI 'ModuleTop where
  sing = SModuleTop

instance SingI 'ModuleLocal where
  sing = SModuleLocal

type LocalModuleName s = SymbolType s

data Module (s :: Stage) (t :: ModuleIsTop) = Module
  { modulePath :: ModulePathType s t,
    moduleParameters :: [InductiveParameter s],
    moduleBody :: [Statement s]
  }

deriving stock instance
  ( Show (ModulePathType s t),
    Show (ModulePathType s 'ModuleLocal),
    Show (ImportType s),
    Show (PatternType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (Module s t)

deriving stock instance
  ( Eq (ModulePathType s t),
    Eq (ModulePathType s 'ModuleLocal),
    Eq (ImportType s),
    Eq (PatternType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (Module s t)

deriving stock instance
  ( Ord (ModulePathType s t),
    Ord (ModulePathType s 'ModuleLocal),
    Ord (ImportType s),
    Ord (PatternType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (Module s t)

data UsingHiding
  = Using (NonEmpty Symbol)
  | Hiding (NonEmpty Symbol)
  deriving stock (Show, Eq, Ord, Data)

data OpenModule (s :: Stage) = OpenModule
  { openModuleName :: NameType s,
    openParameters :: [ExpressionType s],
    openUsingHiding :: Maybe UsingHiding,
    openPublic :: PublicAnn
  }
deriving stock instance
  (
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (PatternType s),
    Eq (ExpressionType s)
  ) =>
  Eq (OpenModule s)

deriving stock instance
  (
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (PatternType s),
    Ord (ExpressionType s)
  ) =>
  Ord (OpenModule s)

deriving stock instance
  (
    Data (NameType s),
    Data (SymbolType s),
    Data (PatternType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (OpenModule s)

deriving stock instance
  (
    Show (NameType s),
    Show (ExpressionType s)
  ) =>
  Show (OpenModule s)

--------------------------------------------------------------------------------
-- Expression
--------------------------------------------------------------------------------

data Expression
  = ExpressionIdentifier (NameType 'Scoped)
  | ExpressionParensIdentifier (NameType 'Scoped)
  | ExpressionApplication Application
  | ExpressionInfixApplication InfixApplication
  | ExpressionPostfixApplication PostfixApplication
  | ExpressionLambda (Lambda 'Scoped)
  | ExpressionMatch (Match 'Scoped)
  | ExpressionLetBlock (LetBlock 'Scoped)
  | ExpressionUniverse Universe
  | ExpressionFunction (Function 'Scoped)
  deriving stock (Show, Eq, Ord, Data)

--------------------------------------------------------------------------------
-- Expression section
--------------------------------------------------------------------------------

-- | Expressions without application
data ExpressionAtom (s :: Stage)
  = AtomIdentifier (NameType s)
  | AtomLambda (Lambda s)
  | AtomLetBlock (LetBlock s)
  | AtomUniverse Universe
  | AtomFunction (Function s)
  | AtomFunArrow
  | AtomMatch (Match s)
  | AtomParens (ExpressionType s)

deriving stock instance
  ( Show (ExpressionType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (PatternType s)
  ) =>
  Show (ExpressionAtom s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (PatternType s)
  ) =>
  Eq (ExpressionAtom s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (PatternType s)
  ) =>
  Ord (ExpressionAtom s)

deriving stock instance
  ( Data (ExpressionType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (PatternType s),
    Typeable s
  ) =>
  Data (ExpressionAtom s)

-- | Expressions without application
newtype ExpressionAtoms (s :: Stage)
  = ExpressionAtoms (NonEmpty (ExpressionAtom s))

deriving stock instance
  ( Show (ExpressionType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (PatternType s)
  ) =>
  Show (ExpressionAtoms s)

deriving stock instance
  ( Eq (ExpressionType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (PatternType s)
  ) =>
  Eq (ExpressionAtoms s)

deriving stock instance
  ( Ord (ExpressionType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (PatternType s)
  ) =>
  Ord (ExpressionAtoms s)

deriving stock instance
  ( Data (ExpressionType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (PatternType s),
   Typeable s
  ) =>
  Data (ExpressionAtoms s)


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

deriving stock instance
  ( Data (ExpressionType s),
    Data (PatternType s),
    Typeable s
  ) =>
  Data (MatchAlt s)


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

deriving stock instance
  ( Data (ExpressionType s),
    Data (PatternType s),
    Typeable s
  ) =>
  Data (Match s)


--------------------------------------------------------------------------------
-- Universe expression
--------------------------------------------------------------------------------

newtype Universe = Universe
  { universeLevel :: Maybe Natural
  }
  deriving stock (Show, Eq, Ord, Data)

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

deriving stock instance (Data (ExpressionType s), Data (SymbolType s), Typeable s) => Data (FunctionParameter s)

data Function (s :: Stage) = Function
  { funParameter :: FunctionParameter s,
    funReturn :: ExpressionType s
  }

deriving stock instance (Show (ExpressionType s), Show (SymbolType s)) => Show (Function s)

deriving stock instance (Eq (ExpressionType s), Eq (SymbolType s)) => Eq (Function s)

deriving stock instance (Ord (ExpressionType s), Ord (SymbolType s)) => Ord (Function s)

deriving stock instance (Data (ExpressionType s), Data (SymbolType s), Typeable s) => Data (Function s)

--------------------------------------------------------------------------------
-- Where block clauses
--------------------------------------------------------------------------------

newtype WhereBlock (s :: Stage) = WhereBlock
  { whereClauses :: NonEmpty (WhereClause s)
  }

deriving stock instance
  ( Show (PatternType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (WhereBlock s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (WhereBlock s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (WhereBlock s)

deriving stock instance
  ( Data (PatternType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (WhereBlock s)

data WhereClause (s :: Stage)
  = WhereOpenModule (OpenModule s)
  | WhereTypeSig (TypeSignature s)
  | WhereFunClause (FunctionClause s)

deriving stock instance
  ( Show (PatternType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (WhereClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (WhereClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (WhereClause s)

deriving stock instance
  ( Data (PatternType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (WhereClause s)

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

deriving stock instance
  ( Data (PatternType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (Lambda s)

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

deriving stock instance
  ( Data (PatternType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (LambdaClause s)

--------------------------------------------------------------------------------
-- Application expression
--------------------------------------------------------------------------------

data Application = Application
  { applicationFunction :: ExpressionType 'Scoped,
    applicationParameter :: ExpressionType 'Scoped
  }
  deriving stock (Show, Eq, Ord, Data)

data InfixApplication = InfixApplication
  { infixAppLeft :: ExpressionType 'Scoped,
    infixAppOperator :: NameType 'Scoped,
    infixAppRight :: ExpressionType 'Scoped
  }
  deriving stock (Show, Eq, Ord, Data)

data PostfixApplication = PostfixApplication
  {
    postfixAppParameter :: ExpressionType 'Scoped,
    postfixAppOperator :: NameType 'Scoped
  }
  deriving stock (Show, Eq, Ord, Data)

--------------------------------------------------------------------------------
-- Let block expression
--------------------------------------------------------------------------------

data LetBlock (s :: Stage) = LetBlock
  { letClauses :: [LetClause s],
    letExpression :: ExpressionType s
  }

deriving stock instance
  ( Show (PatternType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (LetBlock s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LetBlock s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LetBlock s)

deriving stock instance
  ( Data (PatternType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (LetBlock s)

data LetClause (s :: Stage)
  = LetTypeSig (TypeSignature s)
  | LetFunClause (FunctionClause s)

deriving stock instance
  ( Show (PatternType s),
    Show (NameType s),
    Show (SymbolType s),
    Show (ExpressionType s)
  ) =>
  Show (LetClause s)

deriving stock instance
  ( Eq (PatternType s),
    Eq (NameType s),
    Eq (SymbolType s),
    Eq (ExpressionType s)
  ) =>
  Eq (LetClause s)

deriving stock instance
  ( Ord (PatternType s),
    Ord (NameType s),
    Ord (SymbolType s),
    Ord (ExpressionType s)
  ) =>
  Ord (LetClause s)

deriving stock instance
  ( Data (PatternType s),
    Data (NameType s),
    Data (SymbolType s),
    Data (ExpressionType s),
    Typeable s
  ) =>
  Data (LetClause s)

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
