module MiniJuvix.Syntax.Concrete.Scoped.Error.Types
  ( module MiniJuvix.Syntax.Concrete.Language,
    module MiniJuvix.Syntax.Concrete.Scoped.Error.Types,
    module MiniJuvix.Syntax.Concrete.Scoped.Error.Ann,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NonEmpty
import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Language qualified as L
import MiniJuvix.Syntax.Concrete.Parser.Error qualified as Parser
import MiniJuvix.Syntax.Concrete.Scoped.Error.Ann
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base qualified as P
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import Text.EditDistance

class PrettyError e where
  ppError :: e -> Doc Eann

newtype PPOutput = PPOutput (Doc Eann)

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

prettyError :: PrettyError e => e -> AnsiText
prettyError = AnsiText . PPOutput . (<> "ת") . ppError

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

ppSymbolT :: Text -> Doc Eann
ppSymbolT = highlight . pretty

ppCode :: P.PrettyCode c => c -> Doc Eann
ppCode = reAnnotate ScopedAnn . P.runPrettyCode P.defaultOptions

indent' :: Doc ann -> Doc ann
indent' = indent 2

textDistance :: Text -> Text -> Int
textDistance a b =
  restrictedDamerauLevenshteinDistance
    defaultEditCosts
    (unpack a)
    (unpack b)

data MultipleDeclarations = MultipleDeclarations
  { _multipleDeclEntry :: SymbolEntry,
    _multipleDeclSymbol :: Text,
    _multipleDeclSecond :: Interval
  }
  deriving stock (Show)

instance PrettyError MultipleDeclarations where
  ppError MultipleDeclarations {..} =
    "Multiple declarations of" <+> ppSymbolT _multipleDeclSymbol <> line
      <> "Declared at:" <+> align (vsep ints)
    where
      ints = map pretty [L.symbolEntryToSName _multipleDeclEntry ^. S.nameDefined, _multipleDeclSecond]

instance ToGenericError MultipleDeclarations where
  genericError e@MultipleDeclarations {..} =
    Just
      GenericError
        { _genericErrorFile = _multipleDeclSecond ^. intFile,
          _genericErrorLoc = intervalStart i1,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i1, _multipleDeclSecond]
        }
    where
      i1 :: Interval
      i1 = entryName _multipleDeclEntry ^. S.nameDefined

-- | megaparsec error while resolving infixities.
newtype InfixError = InfixError
  { _infixErrAtoms :: ExpressionAtoms 'Scoped
  }
  deriving stock (Show)

-- | megaparsec error while resolving infixities of patterns.
newtype InfixErrorP = InfixErrorP
  { _infixErrAtomsP :: PatternAtom 'Scoped
  }
  deriving stock (Show)

-- | function clause without a type signature.
newtype LacksTypeSig = LacksTypeSig
  { _lacksTypeSigClause :: FunctionClause 'Parsed
  }
  deriving stock (Show)

instance PrettyError LacksTypeSig where
  ppError LacksTypeSig {..} =
    "The declaration is missing a type signature:"
      <> line
      <> indent' (highlight (ppCode _lacksTypeSigClause))

instance ToGenericError LacksTypeSig where
  genericError e@LacksTypeSig {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = _lacksTypeSigClause ^. clauseOwnerFunction . symbolLoc

-- | type signature without a function clause
newtype LacksFunctionClause = LacksFunctionClause
  { _lacksFunctionClause :: TypeSignature 'Scoped
  }
  deriving stock (Show)

instance PrettyError LacksFunctionClause where
  ppError LacksFunctionClause {..} =
    "Type signature with no function clause:"
      <> line
      <> indent' (highlight (ppCode _lacksFunctionClause))

instance ToGenericError LacksFunctionClause where
  genericError e@LacksFunctionClause {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (_lacksFunctionClause ^. sigName)

newtype ImportCycle = ImportCycle
  { -- | If we have [a, b, c] it means that a import b imports c imports a.
    _importCycleImports :: NonEmpty (Import 'Parsed)
  }
  deriving stock (Show)

instance PrettyError ImportCycle where
  ppError ImportCycle {..} =
    "There is an import cycle:" <> line
      <> indent' lst
    where
      lst = vsep $ intersperse "⇓" (map pp (toList (tie _importCycleImports)))
      pp :: Import 'Parsed -> Doc Eann
      pp t = ppCode t <+> parens ("at" <+> pretty (getLoc t))
      tie :: NonEmpty a -> NonEmpty a
      tie x = x <> pure (NonEmpty.head x)

instance ToGenericError ImportCycle where
  genericError e@ImportCycle {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      h = head _importCycleImports
      i = getLoc h

newtype QualSymNotInScope = QualSymNotInScope
  { _qualSymNotInScope :: QualifiedName
  }
  deriving stock (Show)

instance PrettyError QualSymNotInScope where
  ppError QualSymNotInScope {..} =
    "Qualified symbol not in scope:" <+> ppCode _qualSymNotInScope

instance ToGenericError QualSymNotInScope where
  genericError e@QualSymNotInScope {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _qualSymNotInScope

data BindGroupConflict = BindGroupConflict
  { _bindGroupFirst :: Symbol,
    _bindGroupSecond :: Symbol
  }
  deriving stock (Show)

instance PrettyError BindGroupConflict where
  ppError BindGroupConflict {..} =
    "The variable" <+> highlight (ppCode _bindGroupFirst)
      <+> "appears twice in the same binding group:"
      <> line
      <> indent' (align locs)
    where
      locs = vsep $ map (pretty . getLoc) [_bindGroupFirst, _bindGroupSecond]

instance ToGenericError BindGroupConflict where
  genericError e@BindGroupConflict {..} =
    Just
      GenericError
        { _genericErrorFile = i2 ^. intFile,
          _genericErrorLoc = intervalStart i2,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i1, i2]
        }
    where
      i1 = getLoc _bindGroupFirst
      i2 = getLoc _bindGroupSecond

data DuplicateFixity = DuplicateFixity
  { _dupFixityFirst :: OperatorSyntaxDef,
    _dupFixitySecond :: OperatorSyntaxDef
  }
  deriving stock (Show)

instance PrettyError DuplicateFixity where
  ppError DuplicateFixity {..} =
    "Multiple fixity declarations for symbol" <+> highlight (ppCode sym) <> ":" <> line
      <> indent' (align locs)
    where
      sym = _dupFixityFirst ^. opSymbol
      locs = vsep $ map (pretty . getLoc) [_dupFixityFirst, _dupFixityFirst]

instance ToGenericError DuplicateFixity where
  genericError e@DuplicateFixity {..} =
    Just
      GenericError
        { _genericErrorFile = i2 ^. intFile,
          _genericErrorLoc = intervalStart i2,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i1, i2]
        }
    where
      i1 = getLoc _dupFixityFirst
      i2 = getLoc _dupFixitySecond

data MultipleExportConflict = MultipleExportConflict
  { _multipleExportModule :: S.AbsModulePath,
    _multipleExportSymbol :: Symbol,
    _multipleExportEntries :: NonEmpty SymbolEntry
  }
  deriving stock (Show)

instance PrettyError MultipleExportConflict where
  ppError MultipleExportConflict {..} =
    "The symbol" <+> highlight (ppCode _multipleExportSymbol) <+> "is exported multiple times in the module"
      <+> ppCode _multipleExportModule

instance ToGenericError MultipleExportConflict where
  genericError e@MultipleExportConflict {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _multipleExportModule

data NotInScope = NotInScope
  { _notInScopeSymbol :: Symbol,
    _notInScopeLocal :: LocalVars,
    _notInScopeScope :: Scope
  }
  deriving stock (Show)

makeLenses ''NotInScope

instance PrettyError NotInScope where
  ppError NotInScope {..} =
    "Symbol not in scope:" <+> highlight (ppCode _notInScopeSymbol)
      <?> ((line <>) <$> suggestion)
    where
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

instance ToGenericError NotInScope where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = e ^. notInScopeSymbol . symbolLoc . intFile,
          _genericErrorLoc = intervalStart (e ^. notInScopeSymbol . symbolLoc),
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [e ^. notInScopeSymbol . symbolLoc]
        }

instance HasLoc NotInScope where
  getLoc = getLoc . (^. notInScopeSymbol)

newtype ModuleNotInScope = ModuleNotInScope
  { _moduleNotInScopeName :: Name
  }
  deriving stock (Show)

makeLenses ''ModuleNotInScope

instance PrettyError ModuleNotInScope where
  ppError ModuleNotInScope {..} =
    "The module" <+> ppCode _moduleNotInScopeName <+> "is not in scope"

instance ToGenericError ModuleNotInScope where
  genericError e =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc (e ^. moduleNotInScopeName)

newtype MegaParsecError = MegaParsecError
  { _megaParsecError :: Parser.ParserError
  }
  deriving stock (Show)

instance PrettyError MegaParsecError where
  ppError MegaParsecError {..} = pretty _megaParsecError

instance ToGenericError MegaParsecError where
  genericError (MegaParsecError e) = genericError e

newtype UnusedOperatorDef = UnusedOperatorDef
  { _unusedOperatorDef :: OperatorSyntaxDef
  }
  deriving stock (Show)

instance PrettyError UnusedOperatorDef where
  ppError UnusedOperatorDef {..} =
    "Unused operator syntax definition:" <> line
      <> ppCode _unusedOperatorDef

instance ToGenericError UnusedOperatorDef where
  genericError e@UnusedOperatorDef {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _unusedOperatorDef

data WrongTopModuleName = WrongTopModuleName
  { _wrongTopModuleNameExpectedPath :: FilePath,
    _wrongTopModuleNameActualPath :: FilePath,
    _wrongTopModuleNameActualName :: TopModulePath
  }
  deriving stock (Show)

instance PrettyError WrongTopModuleName where
  ppError WrongTopModuleName {..} =
    "The top module" <+> ppCode _wrongTopModuleNameActualName <+> "is defined in the file:" <> line
      <> highlight (pretty _wrongTopModuleNameActualPath)
      <> line
      <> "But it should be in the file:"
      <> line
      <> pretty _wrongTopModuleNameExpectedPath

instance ToGenericError WrongTopModuleName where
  genericError e@WrongTopModuleName {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _wrongTopModuleNameActualName

data AmbiguousSym = AmbiguousSym
  { _ambiguousSymName :: Name,
    _ambiguousSymEntires :: [SymbolEntry]
  }
  deriving stock (Show)

instance PrettyError AmbiguousSym where
  ppError AmbiguousSym {..} = ambiguousMessage _ambiguousSymName _ambiguousSymEntires

instance ToGenericError AmbiguousSym where
  genericError e@AmbiguousSym {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = i : is
        }
    where
      i = getLoc _ambiguousSymName
      is = map getLoc _ambiguousSymEntires

data AmbiguousModuleSym = AmbiguousModuleSym
  { _ambiguousModName :: Name,
    _ambiguousModSymEntires :: [SymbolEntry]
  }
  deriving stock (Show)

instance ToGenericError AmbiguousModuleSym where
  genericError e@AmbiguousModuleSym {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = i : is
        }
    where
      i = getLoc _ambiguousModName
      is = map getLoc _ambiguousModSymEntires

data WrongLocationCompileBlock = WrongLocationCompileBlock
  { _wrongLocationCompileBlockExpectedModPath :: S.AbsModulePath,
    _wrongLocationCompileBlockName :: Name
  }
  deriving stock (Show)

instance PrettyError WrongLocationCompileBlock where
  ppError WrongLocationCompileBlock {..} =
    let name = _wrongLocationCompileBlockName
     in "The compilation rules for the symbol" <+> highlight (ppCode name)
          <+> "need to be defined in the module:"
          <> line
          <> highlight (ppCode _wrongLocationCompileBlockExpectedModPath)

instance ToGenericError WrongLocationCompileBlock where
  genericError e@WrongLocationCompileBlock {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _wrongLocationCompileBlockName

data MultipleCompileBlockSameName = MultipleCompileBlockSameName
  { _multipleCompileBlockFirstDefined :: Interval,
    _multipleCompileBlockSym :: Symbol
  }
  deriving stock (Show)

instance PrettyError MultipleCompileBlockSameName where
  ppError MultipleCompileBlockSameName {..} =
    let name = _multipleCompileBlockSym
     in "Multiple compile blocks for the symbol" <+> highlight (ppCode name)

instance ToGenericError MultipleCompileBlockSameName where
  genericError e@MultipleCompileBlockSameName {..} =
    Just
      GenericError
        { _genericErrorFile = i2 ^. intFile,
          _genericErrorLoc = intervalStart i2,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i1, i2]
        }
    where
      i1 = _multipleCompileBlockFirstDefined
      i2 = getLoc _multipleCompileBlockSym

data MultipleCompileRuleSameBackend = MultipleCompileRuleSameBackend
  { _multipleCompileRuleSameBackendBackendItem :: BackendItem,
    _multipleCompileRuleSameBackendSym :: Symbol
  }
  deriving stock (Show)

instance ToGenericError MultipleCompileRuleSameBackend where
  genericError e@MultipleCompileRuleSameBackend {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _multipleCompileRuleSameBackendSym

data WrongKindExpressionCompileBlock = WrongKindExpressionCompileBlock
  { _wrongKindExpressionCompileBlockSym :: Symbol,
    _wrongKindExpressionCompileBlockEntry :: SymbolEntry
  }
  deriving stock (Show)

instance PrettyError WrongKindExpressionCompileBlock where
  ppError WrongKindExpressionCompileBlock {..} =
    "Symbol" <+> ppCode _wrongKindExpressionCompileBlockSym
      <+> "is not a constructor, inductive, axiom nor a function. Thus, it cannot have a compile rule."

instance ToGenericError WrongKindExpressionCompileBlock where
  genericError e@WrongKindExpressionCompileBlock {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intFile,
          _genericErrorLoc = intervalStart i,
          _genericErrorMessage = prettyError e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc _wrongKindExpressionCompileBlockSym

instance PrettyError InfixError where
  ppError InfixError {..} =
    infixErrorAux "expression" (ppCode _infixErrAtoms)

instance PrettyError InfixErrorP where
  ppError InfixErrorP {..} =
    infixErrorAux "pattern" (ppCode _infixErrAtomsP)

infixErrorAux :: Doc Eann -> Doc Eann -> Doc Eann
infixErrorAux kind pp =
  "Error while resolving infixities in the" <+> kind <> ":" <> line
    <> indent' (highlight pp)

instance PrettyError AmbiguousModuleSym where
  ppError AmbiguousModuleSym {..} = ambiguousMessage _ambiguousModName _ambiguousModSymEntires

instance PrettyError MultipleCompileRuleSameBackend where
  ppError MultipleCompileRuleSameBackend {..} =
    let backend = _multipleCompileRuleSameBackendBackendItem ^. backendItemBackend
        name = _multipleCompileRuleSameBackendSym
     in "Multiple" <+> highlight (ppCode backend) <+> "compilation rules for the symbol"
          <+> highlight (ppCode name)
          <+> "at"
          <+> pretty (getLoc name)

ambiguousMessage :: Name -> [SymbolEntry] -> Doc Eann
ambiguousMessage n es =
  "The symbol" <+> ppCode n <+> "at" <+> pretty (getLoc n) <+> "is ambiguous." <> line
    <> "It could be any of:"
    <> line
    <> indent' (vsep (map ppCode es))
