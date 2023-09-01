{-# LANGUAGE UndecidableInstances #-}

module Juvix.Compiler.Concrete.Language
  ( module Juvix.Compiler.Concrete.Language,
    module Juvix.Compiler.Concrete.Data.Name,
    module Juvix.Compiler.Concrete.Data.Stage,
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
import Juvix.Compiler.Concrete.Data.NameSignature.Base
import Juvix.Compiler.Concrete.Data.NameSpace
import Juvix.Compiler.Concrete.Data.PublicAnn
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Data.Stage
import Juvix.Compiler.Concrete.Data.VisibilityAnn
import Juvix.Data
import Juvix.Data.Ape.Base as Ape
import Juvix.Data.Fixity
import Juvix.Data.FixityInfo (FixityInfo)
import Juvix.Data.IteratorAttribs
import Juvix.Data.Keyword
import Juvix.Data.NameKind
import Juvix.Parser.Lexer (isDelimiterStr)
import Juvix.Prelude hiding (show)
import Juvix.Prelude.Pretty (Pretty, pretty, prettyText)
import Prelude (show)

type Delims = Irrelevant (Maybe (KeywordRef, KeywordRef))

type NameSpaceEntryType :: NameSpace -> GHC.Type
type family NameSpaceEntryType s = res | res -> s where
  NameSpaceEntryType 'NameSpaceSymbols = PreSymbolEntry
  NameSpaceEntryType 'NameSpaceModules = ModuleSymbolEntry
  NameSpaceEntryType 'NameSpaceFixities = FixitySymbolEntry

type RecordUpdateExtraType :: Stage -> GHC.Type
type family RecordUpdateExtraType s = res | res -> s where
  RecordUpdateExtraType 'Parsed = ()
  RecordUpdateExtraType 'Scoped = RecordUpdateExtra

type FieldUpdateArgIxType :: Stage -> GHC.Type
type family FieldUpdateArgIxType s = res | res -> s where
  FieldUpdateArgIxType 'Parsed = ()
  FieldUpdateArgIxType 'Scoped = Int

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

type RecordNameSignatureType :: Stage -> GHC.Type
type family RecordNameSignatureType s = res | res -> s where
  RecordNameSignatureType 'Parsed = ()
  RecordNameSignatureType 'Scoped = RecordNameSignature

type NameSignatureType :: Stage -> GHC.Type
type family NameSignatureType s = res | res -> s where
  NameSignatureType 'Parsed = ()
  NameSignatureType 'Scoped = NameSignature

type ModulePathType :: Stage -> ModuleIsTop -> GHC.Type
type family ModulePathType s t = res | res -> t s where
  ModulePathType 'Parsed 'ModuleTop = TopModulePath
  ModulePathType 'Scoped 'ModuleTop = S.TopModulePath
  ModulePathType 'Parsed 'ModuleLocal = Symbol
  ModulePathType 'Scoped 'ModuleLocal = S.Symbol

type ModuleInductiveType :: ModuleIsTop -> GHC.Type
type family ModuleInductiveType t = res | res -> t where
  ModuleInductiveType 'ModuleTop = ()
  ModuleInductiveType 'ModuleLocal = Bool

type ModuleEndType :: ModuleIsTop -> GHC.Type
type family ModuleEndType t = res | res -> t where
  ModuleEndType 'ModuleTop = ()
  ModuleEndType 'ModuleLocal = KeywordRef

-- | We keep the exact source of the pragma text. This is necessary, because
-- pragmas are supposed to be backwards-compatible. Unrecognised pragmas
-- should be ignored, but they still need to be printed out when
-- pretty-printing. Also, we probably don't want to impose pragma formatting
-- choices on the user.
type ParsedPragmas = WithLoc (WithSource Pragmas)

type ParsedIteratorAttribs = WithLoc (WithSource IteratorAttribs)

type ParsedFixityInfo = WithLoc (WithSource FixityInfo)

data Argument (s :: Stage)
  = ArgumentSymbol (SymbolType s)
  | ArgumentWildcard Wildcard

deriving stock instance Show (Argument 'Parsed)

deriving stock instance Show (Argument 'Scoped)

deriving stock instance Eq (Argument 'Parsed)

deriving stock instance Eq (Argument 'Scoped)

deriving stock instance Ord (Argument 'Parsed)

deriving stock instance Ord (Argument 'Scoped)

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
  = DefinitionSyntax (SyntaxDef s)
  | DefinitionFunctionDef (FunctionDef s)
  | DefinitionInductive (InductiveDef s)
  | DefinitionAxiom (AxiomDef s)
  | DefinitionProjectionDef (ProjectionDef s)

data NonDefinition (s :: Stage)
  = NonDefinitionImport (Import s)
  | NonDefinitionModule (Module s 'ModuleLocal)
  | NonDefinitionOpenModule (OpenModule s)

data Statement (s :: Stage)
  = StatementSyntax (SyntaxDef s)
  | StatementFunctionDef (FunctionDef s)
  | StatementImport (Import s)
  | StatementInductive (InductiveDef s)
  | StatementModule (Module s 'ModuleLocal)
  | StatementOpenModule (OpenModule s)
  | StatementAxiom (AxiomDef s)
  | StatementProjectionDef (ProjectionDef s)

deriving stock instance Show (Statement 'Parsed)

deriving stock instance Show (Statement 'Scoped)

deriving stock instance Eq (Statement 'Parsed)

deriving stock instance Eq (Statement 'Scoped)

deriving stock instance Ord (Statement 'Parsed)

deriving stock instance Ord (Statement 'Scoped)

data ProjectionDef s = ProjectionDef
  { _projectionConstructor :: S.Symbol,
    _projectionField :: SymbolType s,
    _projectionFieldIx :: Int
  }

deriving stock instance Show (ProjectionDef 'Parsed)

deriving stock instance Show (ProjectionDef 'Scoped)

deriving stock instance Eq (ProjectionDef 'Parsed)

deriving stock instance Eq (ProjectionDef 'Scoped)

deriving stock instance Ord (ProjectionDef 'Parsed)

deriving stock instance Ord (ProjectionDef 'Scoped)

data Import (s :: Stage) = Import
  { _importKw :: KeywordRef,
    _importModule :: ImportType s,
    _importAsName :: Maybe (ModulePathType s 'ModuleTop)
  }

deriving stock instance Show (Import 'Parsed)

deriving stock instance Show (Import 'Scoped)

deriving stock instance Eq (Import 'Parsed)

deriving stock instance Eq (Import 'Scoped)

deriving stock instance Ord (Import 'Parsed)

deriving stock instance Ord (Import 'Scoped)

data AliasDef (s :: Stage) = AliasDef
  { _aliasDefSyntaxKw :: Irrelevant KeywordRef,
    _aliasDefAliasKw :: Irrelevant KeywordRef,
    _aliasDefName :: SymbolType s,
    _aliasDefAsName :: IdentifierType s
  }

deriving stock instance (Show (AliasDef 'Parsed))

deriving stock instance (Show (AliasDef 'Scoped))

deriving stock instance (Eq (AliasDef 'Parsed))

deriving stock instance (Eq (AliasDef 'Scoped))

deriving stock instance (Ord (AliasDef 'Parsed))

deriving stock instance (Ord (AliasDef 'Scoped))

data SyntaxDef (s :: Stage)
  = SyntaxFixity (FixitySyntaxDef s)
  | SyntaxOperator OperatorSyntaxDef
  | SyntaxIterator IteratorSyntaxDef
  | SyntaxAlias (AliasDef s)

deriving stock instance (Show (SyntaxDef 'Parsed))

deriving stock instance (Show (SyntaxDef 'Scoped))

deriving stock instance (Eq (SyntaxDef 'Parsed))

deriving stock instance (Eq (SyntaxDef 'Scoped))

deriving stock instance (Ord (SyntaxDef 'Parsed))

deriving stock instance (Ord (SyntaxDef 'Scoped))

data FixitySyntaxDef (s :: Stage) = FixitySyntaxDef
  { _fixitySymbol :: SymbolType s,
    _fixityDoc :: Maybe (Judoc s),
    _fixityInfo :: ParsedFixityInfo,
    _fixityKw :: KeywordRef,
    _fixitySyntaxKw :: KeywordRef
  }

deriving stock instance (Show (FixitySyntaxDef 'Parsed))

deriving stock instance (Show (FixitySyntaxDef 'Scoped))

deriving stock instance (Eq (FixitySyntaxDef 'Parsed))

deriving stock instance (Eq (FixitySyntaxDef 'Scoped))

deriving stock instance (Ord (FixitySyntaxDef 'Parsed))

deriving stock instance (Ord (FixitySyntaxDef 'Scoped))

data FixityDef = FixityDef
  { _fixityDefSymbol :: S.Symbol,
    _fixityDefFixity :: Fixity,
    _fixityDefPrec :: Int
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc (FixitySyntaxDef s) where
  getLoc FixitySyntaxDef {..} = getLoc _fixitySyntaxKw <> getLoc _fixityInfo

data OperatorSyntaxDef = OperatorSyntaxDef
  { _opSymbol :: Symbol,
    _opFixity :: Symbol,
    _opKw :: KeywordRef,
    _opSyntaxKw :: KeywordRef
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc OperatorSyntaxDef where
  getLoc OperatorSyntaxDef {..} = getLoc _opSyntaxKw <> getLoc _opSymbol

data IteratorSyntaxDef = IteratorSyntaxDef
  { _iterSymbol :: Symbol,
    _iterAttribs :: Maybe ParsedIteratorAttribs,
    _iterSyntaxKw :: KeywordRef,
    _iterIteratorKw :: KeywordRef
  }
  deriving stock (Show, Eq, Ord)

instance HasLoc IteratorSyntaxDef where
  getLoc IteratorSyntaxDef {..} = getLoc _iterSyntaxKw <> getLoc _iterSymbol

data SigArgRhs (s :: Stage) = SigArgRhs
  { _sigArgColon :: Irrelevant KeywordRef,
    _sigArgType :: ExpressionType s
  }

deriving stock instance Show (SigArgRhs 'Parsed)

deriving stock instance Show (SigArgRhs 'Scoped)

deriving stock instance Eq (SigArgRhs 'Parsed)

deriving stock instance Eq (SigArgRhs 'Scoped)

deriving stock instance Ord (SigArgRhs 'Parsed)

deriving stock instance Ord (SigArgRhs 'Scoped)

data SigArg (s :: Stage) = SigArg
  { _sigArgDelims :: Irrelevant (KeywordRef, KeywordRef),
    _sigArgImplicit :: IsImplicit,
    _sigArgNames :: NonEmpty (Argument s),
    -- | The rhs is only optional for implicit arguments. Omitting the rhs is
    -- equivalent to writing `: Type`.
    _sigArgRhs :: Maybe (SigArgRhs s)
  }

deriving stock instance Show (SigArg 'Parsed)

deriving stock instance Show (SigArg 'Scoped)

deriving stock instance Eq (SigArg 'Parsed)

deriving stock instance Eq (SigArg 'Scoped)

deriving stock instance Ord (SigArg 'Parsed)

deriving stock instance Ord (SigArg 'Scoped)

data NewFunctionClause (s :: Stage) = NewFunctionClause
  { _clausenPipeKw :: Irrelevant KeywordRef,
    _clausenPatterns :: NonEmpty (PatternAtomType s),
    _clausenAssignKw :: Irrelevant KeywordRef,
    _clausenBody :: ExpressionType s
  }

deriving stock instance Show (NewFunctionClause 'Parsed)

deriving stock instance Show (NewFunctionClause 'Scoped)

deriving stock instance Eq (NewFunctionClause 'Parsed)

deriving stock instance Eq (NewFunctionClause 'Scoped)

deriving stock instance Ord (NewFunctionClause 'Parsed)

deriving stock instance Ord (NewFunctionClause 'Scoped)

data FunctionDefBody (s :: Stage)
  = SigBodyExpression (ExpressionType s)
  | SigBodyClauses (NonEmpty (NewFunctionClause s))

deriving stock instance Show (FunctionDefBody 'Parsed)

deriving stock instance Show (FunctionDefBody 'Scoped)

deriving stock instance Eq (FunctionDefBody 'Parsed)

deriving stock instance Eq (FunctionDefBody 'Scoped)

deriving stock instance Ord (FunctionDefBody 'Parsed)

deriving stock instance Ord (FunctionDefBody 'Scoped)

data FunctionDef (s :: Stage) = FunctionDef
  { _signName :: FunctionName s,
    _signArgs :: [SigArg s],
    _signColonKw :: Irrelevant KeywordRef,
    _signRetType :: ExpressionType s,
    _signDoc :: Maybe (Judoc s),
    _signPragmas :: Maybe ParsedPragmas,
    _signBuiltin :: Maybe (WithLoc BuiltinFunction),
    _signBody :: FunctionDefBody s,
    _signTerminating :: Maybe KeywordRef,
    _signInstance :: Maybe KeywordRef
  }

deriving stock instance Show (FunctionDef 'Parsed)

deriving stock instance Show (FunctionDef 'Scoped)

deriving stock instance Eq (FunctionDef 'Parsed)

deriving stock instance Eq (FunctionDef 'Scoped)

deriving stock instance Ord (FunctionDef 'Parsed)

deriving stock instance Ord (FunctionDef 'Scoped)

data AxiomDef (s :: Stage) = AxiomDef
  { _axiomKw :: Irrelevant KeywordRef,
    _axiomDoc :: Maybe (Judoc s),
    _axiomPragmas :: Maybe ParsedPragmas,
    _axiomName :: SymbolType s,
    _axiomColonKw :: Irrelevant KeywordRef,
    _axiomBuiltin :: Maybe (WithLoc BuiltinAxiom),
    _axiomType :: ExpressionType s
  }

deriving stock instance Show (AxiomDef 'Parsed)

deriving stock instance Show (AxiomDef 'Scoped)

deriving stock instance Eq (AxiomDef 'Parsed)

deriving stock instance Eq (AxiomDef 'Scoped)

deriving stock instance Ord (AxiomDef 'Parsed)

deriving stock instance Ord (AxiomDef 'Scoped)

type InductiveConstructorName s = SymbolType s

type InductiveName s = SymbolType s

data ConstructorDef (s :: Stage) = ConstructorDef
  { _constructorPipe :: Irrelevant (Maybe KeywordRef),
    _constructorName :: InductiveConstructorName s,
    _constructorDoc :: Maybe (Judoc s),
    _constructorPragmas :: Maybe ParsedPragmas,
    _constructorRhs :: ConstructorRhs s
  }

deriving stock instance Show (ConstructorDef 'Parsed)

deriving stock instance Show (ConstructorDef 'Scoped)

deriving stock instance Eq (ConstructorDef 'Parsed)

deriving stock instance Eq (ConstructorDef 'Scoped)

deriving stock instance Ord (ConstructorDef 'Parsed)

deriving stock instance Ord (ConstructorDef 'Scoped)

data RecordUpdateField (s :: Stage) = RecordUpdateField
  { _fieldUpdateName :: Symbol,
    _fieldUpdateArgIx :: FieldUpdateArgIxType s,
    _fieldUpdateAssignKw :: Irrelevant (KeywordRef),
    _fieldUpdateValue :: ExpressionType s
  }

deriving stock instance Show (RecordUpdateField 'Parsed)

deriving stock instance Show (RecordUpdateField 'Scoped)

deriving stock instance Eq (RecordUpdateField 'Parsed)

deriving stock instance Eq (RecordUpdateField 'Scoped)

deriving stock instance Ord (RecordUpdateField 'Parsed)

deriving stock instance Ord (RecordUpdateField 'Scoped)

data RecordField (s :: Stage) = RecordField
  { _fieldName :: SymbolType s,
    _fieldColon :: Irrelevant (KeywordRef),
    _fieldType :: ExpressionType s
  }

deriving stock instance Show (RecordField 'Parsed)

deriving stock instance Show (RecordField 'Scoped)

deriving stock instance Eq (RecordField 'Parsed)

deriving stock instance Eq (RecordField 'Scoped)

deriving stock instance Ord (RecordField 'Parsed)

deriving stock instance Ord (RecordField 'Scoped)

newtype RhsAdt (s :: Stage) = RhsAdt
  { _rhsAdtArguments :: [ExpressionType s]
  }

deriving stock instance Show (RhsAdt 'Parsed)

deriving stock instance Show (RhsAdt 'Scoped)

deriving stock instance Eq (RhsAdt 'Parsed)

deriving stock instance Eq (RhsAdt 'Scoped)

deriving stock instance Ord (RhsAdt 'Parsed)

deriving stock instance Ord (RhsAdt 'Scoped)

data RhsRecord (s :: Stage) = RhsRecord
  { _rhsRecordDelim :: Irrelevant (KeywordRef, KeywordRef),
    _rhsRecordFields :: NonEmpty (RecordField s)
  }

deriving stock instance Show (RhsRecord 'Parsed)

deriving stock instance Show (RhsRecord 'Scoped)

deriving stock instance Eq (RhsRecord 'Parsed)

deriving stock instance Eq (RhsRecord 'Scoped)

deriving stock instance Ord (RhsRecord 'Parsed)

deriving stock instance Ord (RhsRecord 'Scoped)

data RhsGadt (s :: Stage) = RhsGadt
  { _rhsGadtColon :: Irrelevant KeywordRef,
    _rhsGadtType :: ExpressionType s
  }

deriving stock instance Show (RhsGadt 'Parsed)

deriving stock instance Show (RhsGadt 'Scoped)

deriving stock instance Eq (RhsGadt 'Parsed)

deriving stock instance Eq (RhsGadt 'Scoped)

deriving stock instance Ord (RhsGadt 'Parsed)

deriving stock instance Ord (RhsGadt 'Scoped)

data ConstructorRhs (s :: Stage)
  = ConstructorRhsGadt (RhsGadt s)
  | ConstructorRhsRecord (RhsRecord s)
  | ConstructorRhsAdt (RhsAdt s)

deriving stock instance Show (ConstructorRhs 'Parsed)

deriving stock instance Show (ConstructorRhs 'Scoped)

deriving stock instance Eq (ConstructorRhs 'Parsed)

deriving stock instance Eq (ConstructorRhs 'Scoped)

deriving stock instance Ord (ConstructorRhs 'Parsed)

deriving stock instance Ord (ConstructorRhs 'Scoped)

data InductiveParametersRhs (s :: Stage) = InductiveParametersRhs
  { _inductiveParametersColon :: Irrelevant KeywordRef,
    _inductiveParametersType :: ExpressionType s
  }

deriving stock instance Show (InductiveParametersRhs 'Parsed)

deriving stock instance Show (InductiveParametersRhs 'Scoped)

deriving stock instance Eq (InductiveParametersRhs 'Parsed)

deriving stock instance Eq (InductiveParametersRhs 'Scoped)

deriving stock instance Ord (InductiveParametersRhs 'Parsed)

deriving stock instance Ord (InductiveParametersRhs 'Scoped)

data InductiveParameters (s :: Stage) = InductiveParameters
  { _inductiveParametersNames :: NonEmpty (SymbolType s),
    _inductiveParametersRhs :: Maybe (InductiveParametersRhs s)
  }

deriving stock instance Show (InductiveParameters 'Parsed)

deriving stock instance Show (InductiveParameters 'Scoped)

deriving stock instance Eq (InductiveParameters 'Parsed)

deriving stock instance Eq (InductiveParameters 'Scoped)

deriving stock instance Ord (InductiveParameters 'Parsed)

deriving stock instance Ord (InductiveParameters 'Scoped)

data InductiveDef (s :: Stage) = InductiveDef
  { _inductiveKw :: Irrelevant KeywordRef,
    _inductiveAssignKw :: Irrelevant KeywordRef,
    _inductiveBuiltin :: Maybe (WithLoc BuiltinInductive),
    _inductiveDoc :: Maybe (Judoc s),
    _inductivePragmas :: Maybe ParsedPragmas,
    _inductiveName :: InductiveName s,
    _inductiveParameters :: [InductiveParameters s],
    _inductiveType :: Maybe (ExpressionType s),
    _inductiveConstructors :: NonEmpty (ConstructorDef s),
    _inductivePositive :: Maybe KeywordRef,
    _inductiveTrait :: Maybe KeywordRef
  }

deriving stock instance Show (InductiveDef 'Parsed)

deriving stock instance Show (InductiveDef 'Scoped)

deriving stock instance Eq (InductiveDef 'Parsed)

deriving stock instance Eq (InductiveDef 'Scoped)

deriving stock instance Ord (InductiveDef 'Parsed)

deriving stock instance Ord (InductiveDef 'Scoped)

data PatternApp = PatternApp
  { _patAppLeft :: PatternArg,
    _patAppRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord)

data PatternInfixApp = PatternInfixApp
  { _patInfixLeft :: PatternArg,
    _patInfixConstructor :: ScopedIden,
    _patInfixRight :: PatternArg
  }
  deriving stock (Show, Eq, Ord)

data PatternPostfixApp = PatternPostfixApp
  { _patPostfixParameter :: PatternArg,
    _patPostfixConstructor :: ScopedIden
  }
  deriving stock (Show, Eq, Ord)

data PatternArg = PatternArg
  { _patternArgIsImplicit :: IsImplicit,
    _patternArgName :: Maybe S.Symbol,
    _patternArgPattern :: Pattern
  }
  deriving stock (Show, Eq, Ord)

data Pattern
  = PatternVariable (SymbolType 'Scoped)
  | PatternConstructor ScopedIden
  | PatternApplication PatternApp
  | PatternList (ListPattern 'Scoped)
  | PatternInfixApplication PatternInfixApp
  | PatternPostfixApplication PatternPostfixApp
  | PatternWildcard Wildcard
  | PatternEmpty Interval
  | PatternRecord (RecordPattern 'Scoped)
  deriving stock (Show, Eq, Ord)

data PatternScopedIden
  = PatternScopedVar S.Symbol
  | PatternScopedConstructor ScopedIden
  deriving stock (Show, Ord, Eq)

data PatternBinding = PatternBinding
  { _patternBindingName :: Symbol,
    _patternBindingPattern :: PatternAtom 'Parsed
  }
  deriving stock (Ord, Eq, Show)

data ListPattern (s :: Stage) = ListPattern
  { _listpBracketL :: Irrelevant KeywordRef,
    _listpBracketR :: Irrelevant KeywordRef,
    _listpItems :: [PatternParensType s]
  }

deriving stock instance Show (ListPattern 'Parsed)

deriving stock instance Show (ListPattern 'Scoped)

deriving stock instance Eq (ListPattern 'Parsed)

deriving stock instance Eq (ListPattern 'Scoped)

deriving stock instance Ord (ListPattern 'Parsed)

deriving stock instance Ord (ListPattern 'Scoped)

data RecordPatternAssign (s :: Stage) = RecordPatternAssign
  { _recordPatternAssignKw :: Irrelevant KeywordRef,
    _recordPatternAssignField :: Symbol,
    _recordPatternAssignFieldIx :: FieldUpdateArgIxType s,
    _recordPatternAssignPattern :: PatternParensType s
  }

deriving stock instance Show (RecordPatternAssign 'Parsed)

deriving stock instance Show (RecordPatternAssign 'Scoped)

deriving stock instance Eq (RecordPatternAssign 'Parsed)

deriving stock instance Eq (RecordPatternAssign 'Scoped)

deriving stock instance Ord (RecordPatternAssign 'Parsed)

deriving stock instance Ord (RecordPatternAssign 'Scoped)

data FieldPun (s :: Stage) = FieldPun
  { _fieldPunIx :: FieldUpdateArgIxType s,
    _fieldPunField :: SymbolType s
  }

deriving stock instance Show (FieldPun 'Parsed)

deriving stock instance Show (FieldPun 'Scoped)

deriving stock instance Eq (FieldPun 'Parsed)

deriving stock instance Eq (FieldPun 'Scoped)

deriving stock instance Ord (FieldPun 'Parsed)

deriving stock instance Ord (FieldPun 'Scoped)

data RecordPatternItem (s :: Stage)
  = RecordPatternItemFieldPun (FieldPun s)
  | RecordPatternItemAssign (RecordPatternAssign s)

deriving stock instance Show (RecordPatternItem 'Parsed)

deriving stock instance Show (RecordPatternItem 'Scoped)

deriving stock instance Eq (RecordPatternItem 'Parsed)

deriving stock instance Eq (RecordPatternItem 'Scoped)

deriving stock instance Ord (RecordPatternItem 'Parsed)

deriving stock instance Ord (RecordPatternItem 'Scoped)

data RecordPattern (s :: Stage) = RecordPattern
  { _recordPatternConstructor :: IdentifierType s,
    _recordPatternSignature :: Irrelevant (RecordNameSignatureType s),
    _recordPatternItems :: [RecordPatternItem s]
  }

deriving stock instance Show (RecordPattern 'Parsed)

deriving stock instance Show (RecordPattern 'Scoped)

deriving stock instance Eq (RecordPattern 'Parsed)

deriving stock instance Eq (RecordPattern 'Scoped)

deriving stock instance Ord (RecordPattern 'Parsed)

deriving stock instance Ord (RecordPattern 'Scoped)

data PatternAtom (s :: Stage)
  = PatternAtomIden (PatternAtomIdenType s)
  | PatternAtomWildcard Wildcard
  | PatternAtomEmpty Interval
  | PatternAtomList (ListPattern s)
  | PatternAtomRecord (RecordPattern s)
  | PatternAtomParens (PatternParensType s)
  | PatternAtomBraces (PatternParensType s)
  | PatternAtomAt (PatternAtType s)

deriving stock instance Show (PatternAtom 'Parsed)

deriving stock instance Show (PatternAtom 'Scoped)

deriving stock instance Eq (PatternAtom 'Parsed)

deriving stock instance Eq (PatternAtom 'Scoped)

deriving stock instance Ord (PatternAtom 'Parsed)

deriving stock instance Ord (PatternAtom 'Scoped)

data PatternAtoms (s :: Stage) = PatternAtoms
  { _patternAtoms :: NonEmpty (PatternAtom s),
    _patternAtomsLoc :: Irrelevant Interval
  }

deriving stock instance Show (PatternAtoms 'Parsed)

deriving stock instance Show (PatternAtoms 'Scoped)

deriving stock instance Eq (PatternAtoms 'Parsed)

deriving stock instance Eq (PatternAtoms 'Scoped)

deriving stock instance Ord (PatternAtoms 'Parsed)

deriving stock instance Ord (PatternAtoms 'Scoped)

type FunctionName s = SymbolType s

type LocalModuleName s = SymbolType s

data Module (s :: Stage) (t :: ModuleIsTop) = Module
  { _moduleKw :: KeywordRef,
    _modulePath :: ModulePathType s t,
    _moduleDoc :: Maybe (Judoc s),
    _modulePragmas :: Maybe ParsedPragmas,
    _moduleBody :: [Statement s],
    _moduleKwEnd :: ModuleEndType t,
    _moduleInductive :: ModuleInductiveType t
  }

deriving stock instance Show (Module 'Parsed 'ModuleTop)

deriving stock instance Show (Module 'Scoped 'ModuleTop)

deriving stock instance Show (Module 'Parsed 'ModuleLocal)

deriving stock instance Show (Module 'Scoped 'ModuleLocal)

deriving stock instance Eq (Module 'Parsed 'ModuleTop)

deriving stock instance Eq (Module 'Scoped 'ModuleTop)

deriving stock instance Eq (Module 'Parsed 'ModuleLocal)

deriving stock instance Eq (Module 'Scoped 'ModuleLocal)

deriving stock instance Ord (Module 'Parsed 'ModuleTop)

deriving stock instance Ord (Module 'Scoped 'ModuleTop)

deriving stock instance Ord (Module 'Parsed 'ModuleLocal)

deriving stock instance Ord (Module 'Scoped 'ModuleLocal)

data HidingItem (s :: Stage) = HidingItem
  { _hidingSymbol :: SymbolType s,
    _hidingModuleKw :: Maybe KeywordRef
  }

deriving stock instance Show (HidingItem 'Parsed)

deriving stock instance Show (HidingItem 'Scoped)

deriving stock instance Eq (HidingItem 'Parsed)

deriving stock instance Eq (HidingItem 'Scoped)

deriving stock instance Ord (HidingItem 'Parsed)

deriving stock instance Ord (HidingItem 'Scoped)

data UsingItem (s :: Stage) = UsingItem
  { _usingSymbol :: SymbolType s,
    _usingModuleKw :: Maybe KeywordRef,
    _usingAsKw :: Irrelevant (Maybe KeywordRef),
    _usingAs :: Maybe (SymbolType s)
  }

deriving stock instance Show (UsingItem 'Parsed)

deriving stock instance Show (UsingItem 'Scoped)

deriving stock instance Eq (UsingItem 'Parsed)

deriving stock instance Eq (UsingItem 'Scoped)

deriving stock instance Ord (UsingItem 'Parsed)

deriving stock instance Ord (UsingItem 'Scoped)

data UsingList (s :: Stage) = UsingList
  { _usingKw :: Irrelevant KeywordRef,
    _usingBraces :: Irrelevant (KeywordRef, KeywordRef),
    _usingList :: NonEmpty (UsingItem s)
  }

deriving stock instance Show (UsingList 'Parsed)

deriving stock instance Show (UsingList 'Scoped)

deriving stock instance Eq (UsingList 'Parsed)

deriving stock instance Eq (UsingList 'Scoped)

deriving stock instance Ord (UsingList 'Parsed)

deriving stock instance Ord (UsingList 'Scoped)

data HidingList (s :: Stage) = HidingList
  { _hidingKw :: Irrelevant KeywordRef,
    _hidingBraces :: Irrelevant (KeywordRef, KeywordRef),
    _hidingList :: NonEmpty (HidingItem s)
  }

deriving stock instance Show (HidingList 'Parsed)

deriving stock instance Show (HidingList 'Scoped)

deriving stock instance Eq (HidingList 'Parsed)

deriving stock instance Eq (HidingList 'Scoped)

deriving stock instance Ord (HidingList 'Parsed)

deriving stock instance Ord (HidingList 'Scoped)

data UsingHiding (s :: Stage)
  = Using (UsingList s)
  | Hiding (HidingList s)

deriving stock instance Show (UsingHiding 'Parsed)

deriving stock instance Show (UsingHiding 'Scoped)

deriving stock instance Eq (UsingHiding 'Parsed)

deriving stock instance Eq (UsingHiding 'Scoped)

deriving stock instance Ord (UsingHiding 'Parsed)

deriving stock instance Ord (UsingHiding 'Scoped)

type ModuleRef = ModuleRef' 'S.Concrete

newtype ModuleRef' (c :: S.IsConcrete) = ModuleRef'
  { _unModuleRef' :: Σ ModuleIsTop (TyCon1 (ModuleRef'' c))
  }

instance (SingI c) => Show (ModuleRef' c) where
  show = show . getModuleRefNameId

instance (SingI c) => Eq (ModuleRef' c) where
  (==) = (==) `on` getModuleRefNameId

instance (SingI c) => Ord (ModuleRef' c) where
  compare = compare `on` getModuleRefNameId

getNameRefId :: forall c. (SingI c) => RefNameType c -> S.NameId
getNameRefId = case sing :: S.SIsConcrete c of
  S.SConcrete -> (^. S.nameId)
  S.SNotConcrete -> (^. S.nameId)

getModuleRefExportInfo :: ModuleRef' c -> ExportInfo
getModuleRefExportInfo (ModuleRef' (_ :&: ModuleRef'' {..})) = _moduleExportInfo

getModuleRefNameType :: ModuleRef' c -> RefNameType c
getModuleRefNameType (ModuleRef' (_ :&: ModuleRef'' {..})) = _moduleRefName

getModuleRefNameId :: forall c. (SingI c) => ModuleRef' c -> S.NameId
getModuleRefNameId (ModuleRef' (t :&: ModuleRef'' {..})) =
  case sing :: S.SIsConcrete c of
    S.SConcrete -> case t of
      SModuleTop -> _moduleRefName ^. S.nameId
      SModuleLocal -> _moduleRefName ^. S.nameId
    S.SNotConcrete -> _moduleRefName ^. S.nameId

data ModuleRef'' (c :: S.IsConcrete) (t :: ModuleIsTop) = ModuleRef''
  { _moduleRefName :: RefNameType c,
    _moduleExportInfo :: ExportInfo,
    _moduleRefModule :: Module 'Scoped t
  }

instance (Show (RefNameType s)) => Show (ModuleRef'' s t) where
  show ModuleRef'' {..} = show _moduleRefName

instance Eq (ModuleRef'' 'S.Concrete t) where
  (ModuleRef'' n _ _) == (ModuleRef'' n' _ _) = n == n'

instance Ord (ModuleRef'' 'S.Concrete t) where
  compare (ModuleRef'' n _ _) (ModuleRef'' n' _ _) = compare n n'

newtype Alias = Alias
  { _aliasName :: S.Name' ()
  }
  deriving stock (Show)

-- | Either an alias or a symbol entry.
data PreSymbolEntry
  = PreSymbolAlias Alias
  | PreSymbolFinal SymbolEntry
  deriving stock (Show)

-- | A symbol which is not an alias.
newtype SymbolEntry = SymbolEntry
  { _symbolEntry :: S.Name' ()
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable SymbolEntry

newtype ModuleSymbolEntry = ModuleSymbolEntry
  { _moduleEntry :: S.Name' ()
  }
  deriving stock (Show)

newtype FixitySymbolEntry = FixitySymbolEntry
  { _fixityEntry :: S.Name' ()
  }
  deriving stock (Show)

instance (SingI t) => CanonicalProjection (ModuleRef'' c t) (ModuleRef' c) where
  project r = ModuleRef' (sing :&: r)

-- | Symbols that a module exports
data ExportInfo = ExportInfo
  { _exportSymbols :: HashMap Symbol PreSymbolEntry,
    _exportModuleSymbols :: HashMap Symbol ModuleSymbolEntry,
    _exportFixitySymbols :: HashMap Symbol FixitySymbolEntry
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

deriving stock instance Show (OpenModule 'Parsed)

deriving stock instance Show (OpenModule 'Scoped)

deriving stock instance Eq (OpenModule 'Parsed)

deriving stock instance Eq (OpenModule 'Scoped)

deriving stock instance Ord (OpenModule 'Parsed)

deriving stock instance Ord (OpenModule 'Scoped)

data ScopedIden = ScopedIden
  { _scopedIdenFinal :: S.Name,
    _scopedIdenAlias :: Maybe S.Name
  }
  deriving stock (Show, Eq, Ord)

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
  | ExpressionInstanceHole (HoleType 'Scoped)
  | ExpressionRecordUpdate RecordUpdateApp
  | ExpressionParensRecordUpdate ParensRecordUpdate
  | ExpressionBraces (WithLoc Expression)
  | ExpressionDoubleBraces (WithLoc Expression)
  | ExpressionIterator (Iterator 'Scoped)
  | ExpressionNamedApplication (NamedApplication 'Scoped)
  deriving stock (Show, Eq, Ord)

instance HasAtomicity (Lambda s) where
  atomicity = const Atom

data FunctionParameter (s :: Stage)
  = FunctionParameterName (SymbolType s)
  | FunctionParameterWildcard KeywordRef
  -- | Used for traits
  | FunctionParameterUnnamed Interval

deriving stock instance Show (FunctionParameter 'Parsed)

deriving stock instance Show (FunctionParameter 'Scoped)

deriving stock instance Eq (FunctionParameter 'Parsed)

deriving stock instance Eq (FunctionParameter 'Scoped)

deriving stock instance Ord (FunctionParameter 'Parsed)

deriving stock instance Ord (FunctionParameter 'Scoped)

data FunctionParameters (s :: Stage) = FunctionParameters
  { _paramNames :: [FunctionParameter s],
    _paramImplicit :: IsImplicit,
    _paramDelims :: Delims,
    _paramColon :: Irrelevant (Maybe KeywordRef),
    _paramType :: ExpressionType s
  }

deriving stock instance Show (FunctionParameters 'Parsed)

deriving stock instance Show (FunctionParameters 'Scoped)

deriving stock instance Eq (FunctionParameters 'Parsed)

deriving stock instance Eq (FunctionParameters 'Scoped)

deriving stock instance Ord (FunctionParameters 'Parsed)

deriving stock instance Ord (FunctionParameters 'Scoped)

-- | Function *type* representation
data Function (s :: Stage) = Function
  { _funParameters :: FunctionParameters s,
    _funKw :: KeywordRef,
    _funReturn :: ExpressionType s
  }

deriving stock instance Show (Function 'Parsed)

deriving stock instance Show (Function 'Scoped)

deriving stock instance Eq (Function 'Parsed)

deriving stock instance Eq (Function 'Scoped)

deriving stock instance Ord (Function 'Parsed)

deriving stock instance Ord (Function 'Scoped)

data Lambda (s :: Stage) = Lambda
  { _lambdaKw :: KeywordRef,
    _lambdaBraces :: Irrelevant (KeywordRef, KeywordRef),
    _lambdaClauses :: NonEmpty (LambdaClause s)
  }

deriving stock instance Show (Lambda 'Parsed)

deriving stock instance Show (Lambda 'Scoped)

deriving stock instance Eq (Lambda 'Parsed)

deriving stock instance Eq (Lambda 'Scoped)

deriving stock instance Ord (Lambda 'Parsed)

deriving stock instance Ord (Lambda 'Scoped)

data LambdaClause (s :: Stage) = LambdaClause
  { _lambdaPipe :: Irrelevant (Maybe KeywordRef),
    _lambdaParameters :: NonEmpty (PatternAtomType s),
    _lambdaAssignKw :: Irrelevant KeywordRef,
    _lambdaBody :: ExpressionType s
  }

deriving stock instance Show (LambdaClause 'Parsed)

deriving stock instance Show (LambdaClause 'Scoped)

deriving stock instance Eq (LambdaClause 'Parsed)

deriving stock instance Eq (LambdaClause 'Scoped)

deriving stock instance Ord (LambdaClause 'Parsed)

deriving stock instance Ord (LambdaClause 'Scoped)

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

data PostfixApplication = PostfixApplication
  { _postfixAppParameter :: Expression,
    _postfixAppOperator :: ScopedIden
  }
  deriving stock (Show, Eq, Ord)

data LetStatement (s :: Stage)
  = LetFunctionDef (FunctionDef s)
  | LetAliasDef (AliasDef s)

deriving stock instance Show (LetStatement 'Parsed)

deriving stock instance Show (LetStatement 'Scoped)

deriving stock instance Eq (LetStatement 'Parsed)

deriving stock instance Eq (LetStatement 'Scoped)

deriving stock instance Ord (LetStatement 'Parsed)

deriving stock instance Ord (LetStatement 'Scoped)

data Let (s :: Stage) = Let
  { _letKw :: KeywordRef,
    _letInKw :: Irrelevant KeywordRef,
    _letFunDefs :: NonEmpty (LetStatement s),
    _letExpression :: ExpressionType s
  }

deriving stock instance Show (Let 'Parsed)

deriving stock instance Show (Let 'Scoped)

deriving stock instance Eq (Let 'Parsed)

deriving stock instance Eq (Let 'Scoped)

deriving stock instance Ord (Let 'Parsed)

deriving stock instance Ord (Let 'Scoped)

data CaseBranch (s :: Stage) = CaseBranch
  { _caseBranchPipe :: Irrelevant KeywordRef,
    _caseBranchAssignKw :: Irrelevant KeywordRef,
    _caseBranchPattern :: PatternParensType s,
    _caseBranchExpression :: ExpressionType s
  }

deriving stock instance Show (CaseBranch 'Parsed)

deriving stock instance Show (CaseBranch 'Scoped)

deriving stock instance Eq (CaseBranch 'Parsed)

deriving stock instance Eq (CaseBranch 'Scoped)

deriving stock instance Ord (CaseBranch 'Parsed)

deriving stock instance Ord (CaseBranch 'Scoped)

data Case (s :: Stage) = Case
  { _caseKw :: KeywordRef,
    -- | Due to limitations of the pretty printing algorithm, we store whether
    -- the `case` was surrounded by parentheses in the code.
    _caseParens :: Bool,
    _caseExpression :: ExpressionType s,
    _caseBranches :: NonEmpty (CaseBranch s)
  }

deriving stock instance Show (Case 'Parsed)

deriving stock instance Show (Case 'Scoped)

deriving stock instance Eq (Case 'Parsed)

deriving stock instance Eq (Case 'Scoped)

deriving stock instance Ord (Case 'Parsed)

deriving stock instance Ord (Case 'Scoped)

data Initializer (s :: Stage) = Initializer
  { _initializerPattern :: PatternParensType s,
    _initializerAssignKw :: Irrelevant KeywordRef,
    _initializerExpression :: ExpressionType s
  }

deriving stock instance Show (Initializer 'Parsed)

deriving stock instance Show (Initializer 'Scoped)

deriving stock instance Eq (Initializer 'Parsed)

deriving stock instance Eq (Initializer 'Scoped)

deriving stock instance Ord (Initializer 'Parsed)

deriving stock instance Ord (Initializer 'Scoped)

data Range (s :: Stage) = Range
  { _rangePattern :: PatternParensType s,
    _rangeInKw :: Irrelevant KeywordRef,
    _rangeExpression :: ExpressionType s
  }

deriving stock instance Show (Range 'Parsed)

deriving stock instance Show (Range 'Scoped)

deriving stock instance Eq (Range 'Parsed)

deriving stock instance Eq (Range 'Scoped)

deriving stock instance Ord (Range 'Parsed)

deriving stock instance Ord (Range 'Scoped)

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

deriving stock instance Show (Iterator 'Parsed)

deriving stock instance Show (Iterator 'Scoped)

deriving stock instance Eq (Iterator 'Parsed)

deriving stock instance Eq (Iterator 'Scoped)

deriving stock instance Ord (Iterator 'Parsed)

deriving stock instance Ord (Iterator 'Scoped)

data List (s :: Stage) = List
  { _listBracketL :: Irrelevant KeywordRef,
    _listBracketR :: Irrelevant KeywordRef,
    _listItems :: [ExpressionType s]
  }

deriving stock instance Show (List 'Parsed)

deriving stock instance Show (List 'Scoped)

deriving stock instance Eq (List 'Parsed)

deriving stock instance Eq (List 'Scoped)

deriving stock instance Ord (List 'Parsed)

deriving stock instance Ord (List 'Scoped)

data NamedArgument (s :: Stage) = NamedArgument
  { _namedArgName :: Symbol,
    _namedArgAssignKw :: Irrelevant KeywordRef,
    _namedArgValue :: ExpressionType s
  }

deriving stock instance Show (NamedArgument 'Parsed)

deriving stock instance Show (NamedArgument 'Scoped)

deriving stock instance Eq (NamedArgument 'Parsed)

deriving stock instance Eq (NamedArgument 'Scoped)

deriving stock instance Ord (NamedArgument 'Parsed)

deriving stock instance Ord (NamedArgument 'Scoped)

data ArgumentBlock (s :: Stage) = ArgumentBlock
  { _argBlockDelims :: Irrelevant (Maybe (KeywordRef, KeywordRef)),
    _argBlockImplicit :: IsImplicit,
    _argBlockArgs :: NonEmpty (NamedArgument s)
  }

deriving stock instance Show (ArgumentBlock 'Parsed)

deriving stock instance Show (ArgumentBlock 'Scoped)

deriving stock instance Eq (ArgumentBlock 'Parsed)

deriving stock instance Eq (ArgumentBlock 'Scoped)

deriving stock instance Ord (ArgumentBlock 'Parsed)

deriving stock instance Ord (ArgumentBlock 'Scoped)

data RecordUpdateExtra = RecordUpdateExtra
  { _recordUpdateExtraConstructor :: S.Symbol,
    -- | Implicitly bound fields sorted by index
    _recordUpdateExtraVars :: [S.Symbol],
    _recordUpdateExtraSignature :: RecordNameSignature
  }
  deriving stock (Show)

newtype ParensRecordUpdate = ParensRecordUpdate
  { _parensRecordUpdate :: RecordUpdate 'Scoped
  }
  deriving stock (Show, Eq, Ord)

data RecordUpdate (s :: Stage) = RecordUpdate
  { _recordUpdateAtKw :: Irrelevant KeywordRef,
    _recordUpdateDelims :: Irrelevant (KeywordRef, KeywordRef),
    _recordUpdateTypeName :: IdentifierType s,
    _recordUpdateExtra :: Irrelevant (RecordUpdateExtraType s),
    _recordUpdateFields :: NonEmpty (RecordUpdateField s)
  }

deriving stock instance Show (RecordUpdate 'Parsed)

deriving stock instance Show (RecordUpdate 'Scoped)

deriving stock instance Eq (RecordUpdate 'Parsed)

deriving stock instance Eq (RecordUpdate 'Scoped)

deriving stock instance Ord (RecordUpdate 'Parsed)

deriving stock instance Ord (RecordUpdate 'Scoped)

data RecordUpdateApp = RecordUpdateApp
  { _recordAppUpdate :: RecordUpdate 'Scoped,
    _recordAppExpression :: Expression
  }
  deriving stock (Show, Eq, Ord)

data NamedApplication (s :: Stage) = NamedApplication
  { _namedAppName :: IdentifierType s,
    _namedAppArgs :: NonEmpty (ArgumentBlock s),
    _namedAppSignature :: Irrelevant (NameSignatureType s)
  }

deriving stock instance Show (NamedApplication 'Parsed)

deriving stock instance Show (NamedApplication 'Scoped)

deriving stock instance Eq (NamedApplication 'Parsed)

deriving stock instance Eq (NamedApplication 'Scoped)

deriving stock instance Ord (NamedApplication 'Parsed)

deriving stock instance Ord (NamedApplication 'Scoped)

-- | Expressions without application
data ExpressionAtom (s :: Stage)
  = AtomIdentifier (IdentifierType s)
  | AtomLambda (Lambda s)
  | AtomList (List s)
  | AtomCase (Case s)
  | AtomHole (HoleType s)
  | AtomBraces (WithLoc (ExpressionType s))
  | AtomLet (Let s)
  | AtomRecordUpdate (RecordUpdate s)
  | AtomUniverse Universe
  | AtomFunction (Function s)
  | AtomFunArrow KeywordRef
  | AtomLiteral LiteralLoc
  | AtomParens (ExpressionType s)
  | AtomIterator (Iterator s)
  | AtomNamedApplication (NamedApplication s)

deriving stock instance Show (ExpressionAtom 'Parsed)

deriving stock instance Show (ExpressionAtom 'Scoped)

deriving stock instance Eq (ExpressionAtom 'Parsed)

deriving stock instance Eq (ExpressionAtom 'Scoped)

deriving stock instance Ord (ExpressionAtom 'Parsed)

deriving stock instance Ord (ExpressionAtom 'Scoped)

data ExpressionAtoms (s :: Stage) = ExpressionAtoms
  { _expressionAtoms :: NonEmpty (ExpressionAtom s),
    _expressionAtomsLoc :: Irrelevant Interval
  }

deriving stock instance Show (ExpressionAtoms 'Parsed)

deriving stock instance Show (ExpressionAtoms 'Scoped)

deriving stock instance Eq (ExpressionAtoms 'Parsed)

deriving stock instance Eq (ExpressionAtoms 'Scoped)

deriving stock instance Ord (ExpressionAtoms 'Parsed)

deriving stock instance Ord (ExpressionAtoms 'Scoped)

newtype Judoc (s :: Stage) = Judoc
  { _judocGroups :: NonEmpty (JudocGroup s)
  }
  deriving newtype (Semigroup)

deriving stock instance Show (Judoc 'Parsed)

deriving stock instance Show (Judoc 'Scoped)

deriving stock instance Eq (Judoc 'Parsed)

deriving stock instance Eq (Judoc 'Scoped)

deriving stock instance Ord (Judoc 'Parsed)

deriving stock instance Ord (Judoc 'Scoped)

data Example (s :: Stage) = Example
  { _exampleId :: NameId,
    _exampleLoc :: Interval,
    _exampleExpression :: ExpressionType s
  }

deriving stock instance Show (Example 'Parsed)

deriving stock instance Show (Example 'Scoped)

deriving stock instance Eq (Example 'Parsed)

deriving stock instance Eq (Example 'Scoped)

deriving stock instance Ord (Example 'Parsed)

deriving stock instance Ord (Example 'Scoped)

data JudocBlockParagraph (s :: Stage) = JudocBlockParagraph
  { _judocBlockParagraphStart :: KeywordRef,
    _judocBlockParagraphBlocks :: [JudocBlock s],
    _judocBlockParagraphEnd :: KeywordRef
  }

deriving stock instance Show (JudocBlockParagraph 'Parsed)

deriving stock instance Show (JudocBlockParagraph 'Scoped)

deriving stock instance Eq (JudocBlockParagraph 'Parsed)

deriving stock instance Eq (JudocBlockParagraph 'Scoped)

deriving stock instance Ord (JudocBlockParagraph 'Parsed)

deriving stock instance Ord (JudocBlockParagraph 'Scoped)

data JudocGroup (s :: Stage)
  = JudocGroupBlock (JudocBlockParagraph s)
  | JudocGroupLines (NonEmpty (JudocBlock s))

deriving stock instance Show (JudocGroup 'Parsed)

deriving stock instance Show (JudocGroup 'Scoped)

deriving stock instance Eq (JudocGroup 'Parsed)

deriving stock instance Eq (JudocGroup 'Scoped)

deriving stock instance Ord (JudocGroup 'Parsed)

deriving stock instance Ord (JudocGroup 'Scoped)

data JudocBlock (s :: Stage)
  = JudocLines (NonEmpty (JudocLine s))
  | JudocExample (Example s)

deriving stock instance Show (JudocBlock 'Parsed)

deriving stock instance Show (JudocBlock 'Scoped)

deriving stock instance Eq (JudocBlock 'Parsed)

deriving stock instance Eq (JudocBlock 'Scoped)

deriving stock instance Ord (JudocBlock 'Parsed)

deriving stock instance Ord (JudocBlock 'Scoped)

data JudocLine (s :: Stage) = JudocLine
  { _judocLineDelim :: Maybe KeywordRef,
    _judocLineAtoms :: NonEmpty (WithLoc (JudocAtom s))
  }

deriving stock instance Show (JudocLine 'Parsed)

deriving stock instance Show (JudocLine 'Scoped)

deriving stock instance Eq (JudocLine 'Parsed)

deriving stock instance Eq (JudocLine 'Scoped)

deriving stock instance Ord (JudocLine 'Parsed)

deriving stock instance Ord (JudocLine 'Scoped)

data JudocAtom (s :: Stage)
  = JudocExpression (ExpressionType s)
  | JudocText Text

deriving stock instance Show (JudocAtom 'Parsed)

deriving stock instance Show (JudocAtom 'Scoped)

deriving stock instance Eq (JudocAtom 'Parsed)

deriving stock instance Eq (JudocAtom 'Scoped)

deriving stock instance Ord (JudocAtom 'Parsed)

deriving stock instance Ord (JudocAtom 'Scoped)

newtype ModuleIndex = ModuleIndex
  { _moduleIxModule :: Module 'Scoped 'ModuleTop
  }

makeLenses ''PatternArg
makeLenses ''Alias
makeLenses ''FieldPun
makeLenses ''RecordPatternAssign
makeLenses ''RecordPattern
makeLenses ''ParensRecordUpdate
makeLenses ''RecordUpdateExtra
makeLenses ''RecordUpdate
makeLenses ''RecordUpdateApp
makeLenses ''RecordUpdateField
makeLenses ''NonDefinitionsSection
makeLenses ''DefinitionsSection
makeLenses ''ProjectionDef
makeLenses ''ScopedIden
makeLenses ''SymbolEntry
makeLenses ''ModuleSymbolEntry
makeLenses ''FixitySymbolEntry
makeLenses ''FixityDef
makeLenses ''RecordField
makeLenses ''RhsRecord
makeLenses ''RhsAdt
makeLenses ''RhsGadt
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
makeLenses ''FixitySyntaxDef
makeLenses ''OperatorSyntaxDef
makeLenses ''IteratorSyntaxDef
makeLenses ''ConstructorDef
makeLenses ''Module
makeLenses ''SigArg
makeLenses ''SigArgRhs
makeLenses ''FunctionDef
makeLenses ''AxiomDef
makeLenses ''ExportInfo
makeLenses ''InductiveParameters
makeLenses ''InductiveParametersRhs
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
makeLenses ''Initializer
makeLenses ''Range
makeLenses ''ModuleIndex
makeLenses ''ArgumentBlock
makeLenses ''NamedArgument
makeLenses ''NamedApplication
makeLenses ''AliasDef

instance (SingI s) => HasLoc (AliasDef s) where
  getLoc AliasDef {..} = getLoc _aliasDefSyntaxKw <> getLocIdentifierType _aliasDefAsName

instance (SingI s) => HasLoc (SyntaxDef s) where
  getLoc = \case
    SyntaxFixity t -> getLoc t
    SyntaxOperator t -> getLoc t
    SyntaxIterator t -> getLoc t
    SyntaxAlias t -> getLoc t

instance Eq ModuleIndex where
  (==) = (==) `on` (^. moduleIxModule . modulePath)

instance Hashable ModuleIndex where
  hashWithSalt s = hashWithSalt s . (^. moduleIxModule . modulePath)

instance (SingI s) => HasLoc (NamedArgument s) where
  getLoc NamedArgument {..} = getLocSymbolType _namedArgName <> getLocExpressionType _namedArgValue

instance (SingI s) => HasLoc (ArgumentBlock s) where
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
    ExpressionInstanceHole {} -> Atom
    ExpressionParensIdentifier {} -> Atom
    ExpressionApplication {} -> Aggregate appFixity
    ExpressionInfixApplication a -> Aggregate (getFixity a)
    ExpressionPostfixApplication a -> Aggregate (getFixity a)
    ExpressionLambda l -> atomicity l
    ExpressionLiteral l -> atomicity l
    ExpressionLet l -> atomicity l
    ExpressionBraces {} -> Atom
    ExpressionDoubleBraces {} -> Atom
    ExpressionList {} -> Atom
    ExpressionUniverse {} -> Atom
    ExpressionFunction {} -> Aggregate funFixity
    ExpressionCase c -> atomicity c
    ExpressionIterator i -> atomicity i
    ExpressionNamedApplication i -> atomicity i
    ExpressionRecordUpdate {} -> Aggregate updateFixity
    ExpressionParensRecordUpdate {} -> Atom

expressionAtomicity :: forall s. (SingI s) => ExpressionType s -> Atomicity
expressionAtomicity e = case sing :: SStage s of
  SParsed -> atomicity e
  SScoped -> atomicity e

instance HasAtomicity (Iterator s) where
  atomicity = const Atom

instance HasAtomicity (Case s) where
  atomicity = const Atom

instance HasAtomicity (Let 'Scoped) where
  atomicity l = atomicity (l ^. letExpression)

instance HasAtomicity (PatternAtom 'Parsed) where
  atomicity = const Atom

instance (SingI s) => HasAtomicity (FunctionParameters s) where
  atomicity p
    | not (null (p ^. paramNames)) || p ^. paramImplicit == Implicit = Atom
    | otherwise = case sing :: SStage s of
        SParsed -> atomicity (p ^. paramType)
        SScoped -> atomicity (p ^. paramType)

instance Pretty ScopedIden where
  pretty = pretty . (^. scopedIdenName)

instance HasLoc ScopedIden where
  getLoc = getLoc . (^. scopedIdenName)

instance (SingI s) => HasLoc (InductiveParameters s) where
  getLoc i = getLocSymbolType (i ^. inductiveParametersNames . _head1) <>? (getLocExpressionType <$> (i ^? inductiveParametersRhs . _Just . inductiveParametersType))

instance HasLoc (InductiveDef s) where
  getLoc i = (getLoc <$> i ^. inductivePositive) ?<> getLoc (i ^. inductiveKw)

instance HasLoc ModuleRef where
  getLoc (ModuleRef' (_ :&: r)) = getLoc r

instance (SingI s) => HasLoc (AxiomDef s) where
  getLoc m = getLoc (m ^. axiomKw) <> getLocExpressionType (m ^. axiomType)

instance HasLoc (OpenModule 'Scoped) where
  getLoc m =
    getLoc (m ^. openModuleKw)
      <> getLoc (m ^. openModuleName)
      <>? fmap getLoc (m ^. openPublicKw . unIrrelevant)

instance HasLoc (ProjectionDef s) where
  getLoc = getLoc . (^. projectionConstructor)

instance HasLoc (Statement 'Scoped) where
  getLoc :: Statement 'Scoped -> Interval
  getLoc = \case
    StatementSyntax t -> getLoc t
    StatementFunctionDef t -> getLoc t
    StatementImport t -> getLoc t
    StatementInductive t -> getLoc t
    StatementModule t -> getLoc t
    StatementOpenModule t -> getLoc t
    StatementAxiom t -> getLoc t
    StatementProjectionDef t -> getLoc t

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
    FunctionParameterUnnamed i -> i

instance HasLoc (FunctionParameters 'Scoped) where
  getLoc p = case p ^. paramDelims . unIrrelevant of
    Nothing -> (getLoc <$> listToMaybe (p ^. paramNames)) ?<> getLoc (p ^. paramType)
    Just (l, r) -> getLoc l <> getLoc r

instance HasLoc (Function 'Scoped) where
  getLoc f = getLoc (f ^. funParameters) <> getLoc (f ^. funReturn)

instance HasLoc (Let 'Scoped) where
  getLoc l = getLoc (l ^. letKw) <> getLoc (l ^. letExpression)

instance (SingI s) => HasLoc (CaseBranch s) where
  getLoc c = getLoc (c ^. caseBranchPipe) <> getLocExpressionType (c ^. caseBranchExpression)

instance (SingI s) => HasLoc (Case s) where
  getLoc c = getLoc (c ^. caseKw) <> getLoc (c ^. caseBranches . to last)

instance HasLoc (List s) where
  getLoc List {..} = getLoc _listBracketL <> getLoc _listBracketR

instance (SingI s) => HasLoc (NamedApplication s) where
  getLoc NamedApplication {..} = getLocIdentifierType _namedAppName <> getLoc (last _namedAppArgs)

instance (SingI s) => HasLoc (RecordUpdateField s) where
  getLoc f = getLocSymbolType (f ^. fieldUpdateName) <> getLocExpressionType (f ^. fieldUpdateValue)

instance (SingI s) => HasLoc (RecordUpdate s) where
  getLoc r = getLoc (r ^. recordUpdateAtKw) <> getLocSpan (r ^. recordUpdateFields)

instance HasLoc RecordUpdateApp where
  getLoc r = getLoc (r ^. recordAppExpression) <> getLoc (r ^. recordAppUpdate)

instance HasLoc ParensRecordUpdate where
  getLoc = getLoc . (^. parensRecordUpdate)

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
    ExpressionInstanceHole i -> getLoc i
    ExpressionBraces i -> getLoc i
    ExpressionDoubleBraces i -> getLoc i
    ExpressionIterator i -> getLoc i
    ExpressionNamedApplication i -> getLoc i
    ExpressionRecordUpdate i -> getLoc i
    ExpressionParensRecordUpdate i -> getLoc i

getLocIdentifierType :: forall s. (SingI s) => IdentifierType s -> Interval
getLocIdentifierType e = case sing :: SStage s of
  SParsed -> getLoc e
  SScoped -> getLoc e

instance (SingI s) => HasLoc (Iterator s) where
  getLoc Iterator {..} = getLocIdentifierType _iteratorName <> getLocExpressionType _iteratorBody

instance (SingI s) => HasLoc (Import s) where
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

getLocSymbolType :: forall s. (SingI s) => SymbolType s -> Interval
getLocSymbolType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

getLocExpressionType :: forall s. (SingI s) => ExpressionType s -> Interval
getLocExpressionType = case sing :: SStage s of
  SParsed -> getLoc
  SScoped -> getLoc

instance HasLoc (SigArg s) where
  getLoc SigArg {..} = getLoc l <> getLoc r
    where
      Irrelevant (l, r) = _sigArgDelims

instance (SingI s) => HasLoc (NewFunctionClause s) where
  getLoc NewFunctionClause {..} =
    getLoc _clausenPipeKw
      <> getLocExpressionType _clausenBody

instance (SingI s) => HasLoc (FunctionDefBody s) where
  getLoc = \case
    SigBodyExpression e -> getLocExpressionType e
    SigBodyClauses cl -> getLocSpan cl

instance (SingI s) => HasLoc (FunctionDef s) where
  getLoc FunctionDef {..} =
    (getLoc <$> _signDoc)
      ?<> (getLoc <$> _signPragmas)
      ?<> (getLoc <$> _signBuiltin)
      ?<> (getLoc <$> _signTerminating)
      ?<> getLocSymbolType _signName
      <> getLoc _signBody

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

getLocPatternParensType :: forall s. (SingI s) => PatternParensType s -> Interval
getLocPatternParensType = case sing :: SStage s of
  SScoped -> getLoc
  SParsed -> getLoc

instance (SingI s) => HasLoc (RecordPatternAssign s) where
  getLoc a =
    getLoc (a ^. recordPatternAssignField)
      <> getLocPatternParensType (a ^. recordPatternAssignPattern)

instance (SingI s) => HasLoc (FieldPun s) where
  getLoc f = getLocSymbolType (f ^. fieldPunField)

instance (SingI s) => HasLoc (RecordPatternItem s) where
  getLoc = \case
    RecordPatternItemAssign a -> getLoc a
    RecordPatternItemFieldPun a -> getLoc a

instance (SingI s) => HasLoc (RecordPattern s) where
  getLoc r = getLocIdentifierType (r ^. recordPatternConstructor) <>? (getLocSpan <$> nonEmpty (r ^. recordPatternItems))

instance (SingI s) => HasLoc (PatternAtom s) where
  getLoc = \case
    PatternAtomIden i -> getLocIden i
    PatternAtomWildcard w -> getLoc w
    PatternAtomEmpty i -> i
    PatternAtomList l -> getLoc l
    PatternAtomParens p -> getLocParens p
    PatternAtomBraces p -> getLocParens p
    PatternAtomAt p -> getLocAt p
    PatternAtomRecord p -> getLoc p
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
  getLoc = getLoc . (^. patternAtomsLoc)

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
    PatternRecord i -> getLoc i

instance HasLoc (ExpressionAtoms s) where
  getLoc = getLoc . (^. expressionAtomsLoc)

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
          _infixIsDelimiter = isDelimiterStr (prettyText (op ^. scopedIdenName . S.nameConcrete)),
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

instance (SingI s) => IsApe (ArgumentBlock s) ApeLeaf where
  toApe b =
    ApeLeaf
      ( Leaf
          { _leafAtomicity = atomicity b,
            _leafExpr = ApeLeafArgumentBlock (sing :&: b)
          }
      )

toApeIdentifierType :: forall s. (SingI s) => IdentifierType s -> Ape ApeLeaf
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

instance (SingI s) => IsApe (NamedApplication s) ApeLeaf where
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
          _infixIsDelimiter = isDelimiterStr (prettyText (op ^. scopedIdenName . S.nameConcrete)),
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

instance IsApe RecordUpdateApp ApeLeaf where
  toApe :: RecordUpdateApp -> Ape ApeLeaf
  toApe a =
    ApePostfix
      Postfix
        { _postfixFixity = updateFixity,
          _postfixOp = ApeLeafAtom (sing :&: AtomRecordUpdate (a ^. recordAppUpdate)),
          _postfixLeft = toApe (a ^. recordAppExpression)
        }

instance IsApe Expression ApeLeaf where
  toApe e = case e of
    ExpressionApplication a -> toApe a
    ExpressionInfixApplication a -> toApe a
    ExpressionPostfixApplication a -> toApe a
    ExpressionFunction a -> toApe a
    ExpressionNamedApplication a -> toApe a
    ExpressionRecordUpdate a -> toApe a
    ExpressionParensRecordUpdate {} -> leaf
    ExpressionParensIdentifier {} -> leaf
    ExpressionIdentifier {} -> leaf
    ExpressionList {} -> leaf
    ExpressionCase {} -> leaf
    ExpressionLambda {} -> leaf
    ExpressionLet {} -> leaf
    ExpressionUniverse {} -> leaf
    ExpressionHole {} -> leaf
    ExpressionInstanceHole {} -> leaf
    ExpressionLiteral {} -> leaf
    ExpressionBraces {} -> leaf
    ExpressionDoubleBraces {} -> leaf
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

instance HasLoc Alias where
  getLoc = (^. aliasName . S.nameDefined)

instance HasLoc PreSymbolEntry where
  getLoc = \case
    PreSymbolAlias a -> getLoc a
    PreSymbolFinal a -> getLoc a

instance HasLoc SymbolEntry where
  getLoc = (^. symbolEntry . S.nameDefined)

instance HasNameKind ModuleSymbolEntry where
  getNameKind (ModuleSymbolEntry s) = getNameKind s

instance HasLoc ModuleSymbolEntry where
  getLoc (ModuleSymbolEntry s) = s ^. S.nameDefined

overModuleRef'' :: forall s s'. (forall t. ModuleRef'' s t -> ModuleRef'' s' t) -> ModuleRef' s -> ModuleRef' s'
overModuleRef'' f = over unModuleRef' (\(t :&: m'') -> t :&: f m'')

symbolEntryNameId :: SymbolEntry -> NameId
symbolEntryNameId = (^. symbolEntry . S.nameId)

instance HasNameKind ScopedIden where
  getNameKind = getNameKind . (^. scopedIdenFinal)

instance HasNameKind SymbolEntry where
  getNameKind = getNameKind . (^. symbolEntry)

exportAllNames :: SimpleFold ExportInfo (S.Name' ())
exportAllNames =
  exportSymbols
    . each
    . preSymbolName
    <> exportModuleSymbols
      . each
      . moduleEntry
    <> exportFixitySymbols
      . each
      . fixityEntry

exportNameSpace :: forall ns. (SingI ns) => Lens' ExportInfo (HashMap Symbol (NameSpaceEntryType ns))
exportNameSpace = case sing :: SNameSpace ns of
  SNameSpaceSymbols -> exportSymbols
  SNameSpaceModules -> exportModuleSymbols
  SNameSpaceFixities -> exportFixitySymbols

_ConstructorRhsRecord :: Traversal' (ConstructorRhs s) (RhsRecord s)
_ConstructorRhsRecord f rhs = case rhs of
  ConstructorRhsRecord r -> ConstructorRhsRecord <$> f r
  _ -> pure rhs

_DefinitionSyntax :: Traversal' (Definition s) (SyntaxDef s)
_DefinitionSyntax f x = case x of
  DefinitionSyntax r -> DefinitionSyntax <$> f r
  _ -> pure x

_SyntaxAlias :: Traversal' (SyntaxDef s) (AliasDef s)
_SyntaxAlias f x = case x of
  SyntaxAlias r -> SyntaxAlias <$> f r
  _ -> pure x

scopedIdenName :: Lens' ScopedIden S.Name
scopedIdenName f n = case n ^. scopedIdenAlias of
  Nothing -> scopedIdenFinal f n
  Just a -> do
    a' <- f a
    pure (set scopedIdenAlias (Just a') n)

instance HasFixity PostfixApplication where
  getFixity (PostfixApplication _ op) = fromMaybe impossible (op ^. scopedIdenName . S.nameFixity)

instance HasFixity InfixApplication where
  getFixity (InfixApplication _ op _) = fromMaybe impossible (op ^. scopedIdenName . S.nameFixity)

preSymbolName :: Lens' PreSymbolEntry (S.Name' ())
preSymbolName f = \case
  PreSymbolAlias a -> PreSymbolAlias <$> traverseOf aliasName f a
  PreSymbolFinal a -> PreSymbolFinal <$> traverseOf symbolEntry f a

instance HasFixity PatternInfixApp where
  getFixity (PatternInfixApp _ op _) = fromMaybe impossible (op ^. scopedIdenName . S.nameFixity)

instance HasFixity PatternPostfixApp where
  getFixity (PatternPostfixApp _ op) = fromMaybe impossible (op ^. scopedIdenName . S.nameFixity)

instance HasAtomicity (ListPattern s) where
  atomicity = const Atom

instance HasAtomicity (RecordPattern s) where
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
    PatternRecord r -> atomicity r
