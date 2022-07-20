module Juvix.Syntax.Concrete.Scoped.Error.Types
  ( module Juvix.Syntax.Concrete.Language,
    module Juvix.Syntax.Concrete.Scoped.Error.Types,
    module Juvix.Syntax.Concrete.Scoped.Error.Ann,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Prelude
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Language
import Juvix.Syntax.Concrete.Language qualified as L
import Juvix.Syntax.Concrete.Parser.Error qualified as Parser
import Juvix.Syntax.Concrete.Scoped.Error.Ann
import Juvix.Syntax.Concrete.Scoped.Error.Pretty
import Juvix.Syntax.Concrete.Scoped.Name qualified as S
import Juvix.Syntax.Concrete.Scoped.Scope

data MultipleDeclarations = MultipleDeclarations
  { _multipleDeclEntry :: SymbolEntry,
    _multipleDeclSymbol :: Text,
    _multipleDeclSecond :: Interval
  }
  deriving stock (Show)

instance ToGenericError MultipleDeclarations where
  genericError MultipleDeclarations {..} =
    GenericError
      { _genericErrorLoc = i1,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i1, _multipleDeclSecond]
      }
    where
      i1 :: Interval
      i1 = entryName _multipleDeclEntry ^. S.nameDefined
      msg =
        "Multiple declarations of"
          <+> ppSymbolT _multipleDeclSymbol
            <> line
            <> "Declared at:"
          <+> align (vsep lst)
      lst = map pretty [L.symbolEntryToSName _multipleDeclEntry ^. S.nameDefined, _multipleDeclSecond]

-- | megaparsec error while resolving infixities.
newtype InfixError = InfixError
  { _infixErrorAtoms :: ExpressionAtoms 'Scoped
  }
  deriving stock (Show)

instance ToGenericError InfixError where
  genericError InfixError {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _infixErrorAtoms
      msg :: Doc Eann
      msg =
        "Error solving infixities"
          <> line
          <> indent' (highlight (ppCode _infixErrorAtoms))

-- | megaparsec error while resolving infixities of patterns.
newtype InfixErrorP = InfixErrorP
  { _infixErrorAtomsP :: PatternAtoms 'Scoped
  }
  deriving stock (Show)

instance ToGenericError InfixErrorP where
  genericError InfixErrorP {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _infixErrorAtomsP
      msg :: Doc Eann
      msg =
        "Error solving infixities:"
          <> line
          <> indent' (highlight (ppCode _infixErrorAtomsP))

-- | function clause without a type signature.
newtype LacksTypeSig = LacksTypeSig
  { _lacksTypeSigClause :: FunctionClause 'Parsed
  }
  deriving stock (Show)

instance ToGenericError LacksTypeSig where
  genericError LacksTypeSig {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = _lacksTypeSigClause ^. clauseOwnerFunction . symbolLoc
      msg =
        "The declaration is missing a type signature:"
          <> line
          <> indent' (highlight (ppCode _lacksTypeSigClause))

-- | type signature without a function clause
newtype LacksFunctionClause = LacksFunctionClause
  { _lacksFunctionClause :: TypeSignature 'Scoped
  }
  deriving stock (Show)

instance ToGenericError LacksFunctionClause where
  genericError LacksFunctionClause {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (_lacksFunctionClause ^. sigName)
      msg =
        "Type signature with no function clause:"
          <> line
          <> indent' (highlight (ppCode _lacksFunctionClause))

newtype ImportCycle = ImportCycle
  { -- | If we have [a, b, c] it means that a import b imports c imports a.
    _importCycleImports :: NonEmpty (Import 'Parsed)
  }
  deriving stock (Show)

instance ToGenericError ImportCycle where
  genericError ImportCycle {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      h = head _importCycleImports
      i = getLoc h
      msg =
        "There is an import cycle:"
          <> line
          <> indent' (vsep (intersperse "⇓" (map pp (toList (tie _importCycleImports)))))

      pp :: Import 'Parsed -> Doc Eann
      pp t = ppCode t <+> parens ("at" <+> pretty (getLoc t))

      tie :: NonEmpty a -> NonEmpty a
      tie x = x <> pure (NonEmpty.head x)

newtype QualSymNotInScope = QualSymNotInScope
  { _qualSymNotInScope :: QualifiedName
  }
  deriving stock (Show)

instance ToGenericError QualSymNotInScope where
  genericError QualSymNotInScope {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _qualSymNotInScope
      msg = "Qualified symbol not in scope:" <+> ppCode _qualSymNotInScope

data BindGroupConflict = BindGroupConflict
  { _bindGroupFirst :: Symbol,
    _bindGroupSecond :: Symbol
  }
  deriving stock (Show)

instance ToGenericError BindGroupConflict where
  genericError BindGroupConflict {..} =
    GenericError
      { _genericErrorLoc = i2,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i1, i2]
      }
    where
      i1 = getLoc _bindGroupFirst
      i2 = getLoc _bindGroupSecond

      msg =
        "The variable"
          <+> highlight (ppCode _bindGroupFirst)
          <+> "appears twice in the same binding group:"
            <> line
            <> indent' (align (vsep (map pretty [i1, i2])))

data DuplicateFixity = DuplicateFixity
  { _dupFixityFirst :: OperatorSyntaxDef,
    _dupFixitySecond :: OperatorSyntaxDef
  }
  deriving stock (Show)

instance ToGenericError DuplicateFixity where
  genericError DuplicateFixity {..} =
    GenericError
      { _genericErrorLoc = i2,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i1, i2]
      }
    where
      i1 = getLoc _dupFixityFirst
      i2 = getLoc _dupFixitySecond

      msg =
        "Multiple fixity declarations for symbol"
          <+> highlight (ppCode sym)
            <> ":"
            <> line
            <> indent' (align locs)
        where
          sym = _dupFixityFirst ^. opSymbol
          locs = vsep $ map (pretty . getLoc) [_dupFixityFirst, _dupFixityFirst]

data MultipleExportConflict = MultipleExportConflict
  { _multipleExportModule :: S.AbsModulePath,
    _multipleExportSymbol :: Symbol,
    _multipleExportEntries :: NonEmpty SymbolEntry
  }
  deriving stock (Show)

instance ToGenericError MultipleExportConflict where
  genericError MultipleExportConflict {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _multipleExportModule
      msg =
        "The symbol"
          <+> highlight (ppCode _multipleExportSymbol)
          <+> "is exported multiple times in the module"
          <+> ppCode _multipleExportModule

data NotInScope = NotInScope
  { _notInScopeSymbol :: Symbol,
    _notInScopeLocal :: LocalVars,
    _notInScopeScope :: Scope
  }
  deriving stock (Show)

makeLenses ''NotInScope

instance ToGenericError NotInScope where
  genericError e@NotInScope {..} =
    GenericError
      { _genericErrorLoc = e ^. notInScopeSymbol . symbolLoc,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [e ^. notInScopeSymbol . symbolLoc]
      }
    where
      msg =
        "Symbol not in scope:" <+> highlight (ppCode _notInScopeSymbol)
          <?> ((line <>) <$> suggestion)
      suggestion :: Maybe (Doc a)
      suggestion
        | null suggestions = Nothing
        | otherwise = Just $ "Perhaps you meant:" <+> align (vsep suggestions)
      sym = _notInScopeSymbol ^. symbolText
      maxDist :: Int
      maxDist = 2
      suggestions :: [Doc a]
      suggestions =
        map (pretty . fst) $
          sortOn
            snd
            [ (c, dist) | c <- toList candidates, let dist = textDistance sym c, dist <= maxDist
            ]
      candidates :: HashSet Text
      candidates =
        HashSet.fromList (map (^. symbolText) (HashMap.keys (_notInScopeLocal ^. localVars)))
          <> HashSet.fromList (map (^. symbolText) (HashMap.keys (_notInScopeScope ^. scopeSymbols)))

instance HasLoc NotInScope where
  getLoc = getLoc . (^. notInScopeSymbol)

newtype AppLeftImplicit = AppLeftImplicit
  { _appLeftImplicit :: WithLoc Expression
  }
  deriving stock (Show)

makeLenses ''AppLeftImplicit

instance ToGenericError AppLeftImplicit where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. appLeftImplicit)
      msg =
        "The expression"
          <+> ppCode (e ^. appLeftImplicit)
          <+> "cannot appear by itself."
            <> line
            <> "It needs to be the argument of a function expecting an implicit argument."

newtype ModuleNotInScope = ModuleNotInScope
  { _moduleNotInScopeName :: Name
  }
  deriving stock (Show)

makeLenses ''ModuleNotInScope

instance ToGenericError ModuleNotInScope where
  genericError e@ModuleNotInScope {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. moduleNotInScopeName)
      msg = "The module" <+> ppCode _moduleNotInScopeName <+> "is not in scope"

newtype MegaParsecError = MegaParsecError
  { _megaParsecError :: Parser.ParserError
  }
  deriving stock (Show)

instance ToGenericError MegaParsecError where
  genericError (MegaParsecError e) = genericError e

newtype UnusedOperatorDef = UnusedOperatorDef
  { _unusedOperatorDef :: OperatorSyntaxDef
  }
  deriving stock (Show)

instance ToGenericError UnusedOperatorDef where
  genericError UnusedOperatorDef {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _unusedOperatorDef
      msg =
        "Unused operator syntax definition:"
          <> line
          <> ppCode _unusedOperatorDef

data WrongTopModuleName = WrongTopModuleName
  { _wrongTopModuleNameExpectedPath :: FilePath,
    _wrongTopModuleNameActualPath :: FilePath,
    _wrongTopModuleNameActualName :: TopModulePath
  }
  deriving stock (Show)

instance ToGenericError WrongTopModuleName where
  genericError WrongTopModuleName {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _wrongTopModuleNameActualName
      msg =
        "The top module"
          <+> ppCode _wrongTopModuleNameActualName
          <+> "is defined in the file:"
            <> line
            <> highlight (pretty _wrongTopModuleNameActualPath)
            <> line
            <> "But it should be in the file:"
            <> line
            <> pretty _wrongTopModuleNameExpectedPath

data AmbiguousSym = AmbiguousSym
  { _ambiguousSymName :: Name,
    _ambiguousSymEntires :: [SymbolEntry]
  }
  deriving stock (Show)

instance ToGenericError AmbiguousSym where
  genericError AmbiguousSym {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = i : is
      }
    where
      i = getLoc _ambiguousSymName
      is = map getLoc _ambiguousSymEntires
      msg = ambiguousMessage _ambiguousSymName _ambiguousSymEntires

data AmbiguousModuleSym = AmbiguousModuleSym
  { _ambiguousModName :: Name,
    _ambiguousModSymEntires :: [SymbolEntry]
  }
  deriving stock (Show)

instance ToGenericError AmbiguousModuleSym where
  genericError AmbiguousModuleSym {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = i : is
      }
    where
      i = getLoc _ambiguousModName
      is = map getLoc _ambiguousModSymEntires
      msg = ambiguousMessage _ambiguousModName _ambiguousModSymEntires

data WrongLocationCompileBlock = WrongLocationCompileBlock
  { _wrongLocationCompileBlockExpectedModPath :: S.AbsModulePath,
    _wrongLocationCompileBlockName :: Name
  }
  deriving stock (Show)

instance ToGenericError WrongLocationCompileBlock where
  genericError WrongLocationCompileBlock {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      name = _wrongLocationCompileBlockName
      i = getLoc name
      msg =
        "The compilation rules for the symbol"
          <+> highlight (ppCode name)
          <+> "need to be defined in the module:"
            <> line
            <> highlight (ppCode _wrongLocationCompileBlockExpectedModPath)

data MultipleCompileBlockSameName = MultipleCompileBlockSameName
  { _multipleCompileBlockFirstDefined :: Interval,
    _multipleCompileBlockSym :: Symbol
  }
  deriving stock (Show)

instance ToGenericError MultipleCompileBlockSameName where
  genericError MultipleCompileBlockSameName {..} =
    GenericError
      { _genericErrorLoc = i2,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i1, i2]
      }
    where
      i1 = _multipleCompileBlockFirstDefined
      i2 = getLoc _multipleCompileBlockSym
      msg =
        "Multiple compile blocks for the symbol"
          <+> highlight (ppCode _multipleCompileBlockSym)

data MultipleCompileRuleSameBackend = MultipleCompileRuleSameBackend
  { _multipleCompileRuleSameBackendBackendItem :: BackendItem,
    _multipleCompileRuleSameBackendSym :: Symbol
  }
  deriving stock (Show)

instance ToGenericError MultipleCompileRuleSameBackend where
  genericError MultipleCompileRuleSameBackend {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      backend = _multipleCompileRuleSameBackendBackendItem ^. backendItemBackend
      name = _multipleCompileRuleSameBackendSym
      i = getLoc _multipleCompileRuleSameBackendSym
      msg =
        "Multiple"
          <+> highlight (ppCode backend)
          <+> "compilation rules for the symbol"
          <+> highlight (ppCode name)
          <+> "at"
          <+> pretty (getLoc name)

data WrongKindExpressionCompileBlock = WrongKindExpressionCompileBlock
  { _wrongKindExpressionCompileBlockSym :: Symbol,
    _wrongKindExpressionCompileBlockEntry :: SymbolEntry
  }
  deriving stock (Show)

instance ToGenericError WrongKindExpressionCompileBlock where
  genericError WrongKindExpressionCompileBlock {..} =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc _wrongKindExpressionCompileBlockSym
      msg =
        "Symbol"
          <+> ppCode _wrongKindExpressionCompileBlockSym
          <+> "is not a constructor, inductive data type, axiom nor a function."
            <> "Thus, it cannot have a compile rule."

infixErrorAux :: Doc Eann -> Doc Eann -> Doc Eann
infixErrorAux kind pp =
  "Error while resolving infixities in the"
    <+> kind
      <> ":"
      <> line
      <> indent' (highlight pp)

ambiguousMessage :: Name -> [SymbolEntry] -> Doc Eann
ambiguousMessage n es =
  "The symbol"
    <+> ppCode n
    <+> "at"
    <+> pretty (getLoc n)
    <+> "is ambiguous."
      <> line
      <> "It could be any of:"
      <> line
      <> indent' (vsep (map ppCode es))

newtype DuplicateInductiveParameterName = DuplicateInductiveParameterName
  { _duplicateInductiveParameterName :: Symbol
  }
  deriving stock (Show)

makeLenses ''DuplicateInductiveParameterName

instance ToGenericError DuplicateInductiveParameterName where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      param = e ^. duplicateInductiveParameterName
      i = getLoc param
      msg =
        "Invalid name"
          <+> ppCode param
            <> "."
            <> line
            <> "Inductive parameter names can not be repeated."

newtype DoubleBracesPattern = DoubleBracesPattern
  { _doubleBracesPatternArg :: PatternArg
  }
  deriving stock (Show)

makeLenses ''DoubleBracesPattern

instance ToGenericError DoubleBracesPattern where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      pat :: PatternArg
      pat = e ^. doubleBracesPatternArg
      i = getLoc pat
      msg =
        "Double braces are not valid:"
          -- TODO add bold to braces
          <+> braces (ppCode pat)

newtype ImplicitPatternLeftApplication = ImplicitPatternLeftApplication
  { _implicitPatternLeftApplication :: PatternApp
  }
  deriving stock (Show)

makeLenses ''ImplicitPatternLeftApplication

instance ToGenericError ImplicitPatternLeftApplication where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      pat :: PatternApp
      pat = e ^. implicitPatternLeftApplication
      i = getLoc pat
      msg =
        "Pattern matching an implicit argument cannot occur on the left of an application:"
          <+> ppCode pat

newtype ConstructorExpectedLeftApplication = ConstructorExpectedLeftApplication
  { _constructorExpectedLeftApplicationPattern :: Pattern
  }
  deriving stock (Show)

makeLenses ''ConstructorExpectedLeftApplication

instance ToGenericError ConstructorExpectedLeftApplication where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      pat :: Pattern
      pat = e ^. constructorExpectedLeftApplicationPattern
      i = getLoc pat
      msg =
        "Constructor expected on the left of a pattern application:"
          <+> ppCode pat
