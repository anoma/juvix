module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types
  ( module Juvix.Compiler.Concrete.Language,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types,
    module Juvix.Data.CodeAnn,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Language qualified as L
import Juvix.Compiler.Concrete.Pretty.Options (Options, fromGenericOptions)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Data.CodeAnn
import Juvix.Prelude

data MultipleDeclarations = MultipleDeclarations
  { _multipleDeclEntry :: SymbolEntry,
    _multipleDeclSymbol :: Text,
    _multipleDeclSecond :: Interval
  }
  deriving stock (Show)

instance ToGenericError MultipleDeclarations where
  genericError MultipleDeclarations {..} =
    return
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
  genericError InfixError {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _infixErrorAtoms
          msg :: Doc Ann
          msg =
            "Error solving infixities"
              <> line
              <> indent' (ppCode opts' _infixErrorAtoms)

-- | megaparsec error while resolving infixities of patterns.
newtype InfixErrorP = InfixErrorP
  { _infixErrorAtomsP :: PatternAtoms 'Scoped
  }
  deriving stock (Show)

instance ToGenericError InfixErrorP where
  genericError InfixErrorP {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _infixErrorAtomsP
          msg :: Doc Ann
          msg =
            "Error solving infixities:"
              <> line
              <> indent' (ppCode opts' _infixErrorAtomsP)

-- | function clause without a type signature.
newtype LacksTypeSig = LacksTypeSig
  { _lacksTypeSigClause :: FunctionClause 'Parsed
  }
  deriving stock (Show)

instance ToGenericError LacksTypeSig where
  genericError LacksTypeSig {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = _lacksTypeSigClause ^. clauseOwnerFunction . symbolLoc
          msg =
            "The declaration is missing a type signature:"
              <> line
              <> indent' (ppCode opts' _lacksTypeSigClause)

-- | type signature without a function clause
newtype LacksFunctionClause = LacksFunctionClause
  { _lacksFunctionClause :: TypeSignature 'Scoped
  }
  deriving stock (Show)

instance ToGenericError LacksFunctionClause where
  genericError LacksFunctionClause {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (_lacksFunctionClause ^. sigName)
          msg =
            "Type signature with no function clause:"
              <> line
              <> indent' (ppCode opts' _lacksFunctionClause)

newtype ImportCycle = ImportCycle
  { -- | If we have [a, b, c] it means that a import b imports c imports a.
    _importCycleImports :: NonEmpty (Import 'Parsed)
  }
  deriving stock (Show)

instance ToGenericError ImportCycle where
  genericError ImportCycle {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          h = head _importCycleImports
          i = getLoc h
          msg =
            "There is an import cycle:"
              <> line
              <> indent' (vsep (intersperse "⇓" (map pp (toList (tie _importCycleImports)))))

          pp :: Import 'Parsed -> Doc Ann
          pp t = ppCode opts' t <+> parens ("at" <+> pretty (getLoc t))

          tie :: NonEmpty a -> NonEmpty a
          tie x = x <> pure (NonEmpty.head x)

newtype QualSymNotInScope = QualSymNotInScope
  { _qualSymNotInScope :: QualifiedName
  }
  deriving stock (Show)

instance ToGenericError QualSymNotInScope where
  genericError QualSymNotInScope {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _qualSymNotInScope
          msg = "Qualified symbol not in scope:" <+> ppCode opts' _qualSymNotInScope

data BindGroupConflict = BindGroupConflict
  { _bindGroupFirst :: Symbol,
    _bindGroupSecond :: Symbol
  }
  deriving stock (Show)

instance ToGenericError BindGroupConflict where
  genericError BindGroupConflict {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i2,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i1, i2]
            }
        where
          opts' = fromGenericOptions opts
          i1 = getLoc _bindGroupFirst
          i2 = getLoc _bindGroupSecond

          msg =
            "The variable"
              <+> ppCode opts' _bindGroupFirst
              <+> "appears twice in the same binding group:"
                <> line
                <> indent' (align (vsep (map pretty [i1, i2])))

data DuplicateFixity = DuplicateFixity
  { _dupFixityFirst :: OperatorSyntaxDef,
    _dupFixitySecond :: OperatorSyntaxDef
  }
  deriving stock (Show)

instance ToGenericError DuplicateFixity where
  genericError DuplicateFixity {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i2,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i1, i2]
            }
        where
          opts' = fromGenericOptions opts
          i1 = getLoc _dupFixityFirst
          i2 = getLoc _dupFixitySecond

          msg =
            "Multiple fixity declarations for symbol"
              <+> ppCode opts' sym
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
  genericError MultipleExportConflict {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _multipleExportModule
          msg =
            "The symbol"
              <+> ppCode opts' _multipleExportSymbol
              <+> "is exported multiple times in the module"
              <+> ppCode opts' _multipleExportModule

data NotInScope = NotInScope
  { _notInScopeSymbol :: Symbol,
    _notInScopeLocal :: LocalVars,
    _notInScopeScope :: Scope
  }
  deriving stock (Show)

makeLenses ''NotInScope

instance ToGenericError NotInScope where
  genericError e@NotInScope {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = e ^. notInScopeSymbol . symbolLoc,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [e ^. notInScopeSymbol . symbolLoc]
            }
        where
          opts' = fromGenericOptions opts
          msg =
            "Symbol not in scope:"
              <+> ppCode opts' _notInScopeSymbol
                <>? ((line <>) <$> suggestion)
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
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. appLeftImplicit)
          msg =
            "The expression"
              <+> ppCode opts' (e ^. appLeftImplicit)
              <+> "cannot appear by itself."
                <> line
                <> "It needs to be the argument of a function expecting an implicit argument."

newtype ModuleNotInScope = ModuleNotInScope
  { _moduleNotInScopeName :: Name
  }
  deriving stock (Show)

makeLenses ''ModuleNotInScope

instance ToGenericError ModuleNotInScope where
  genericError e@ModuleNotInScope {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc (e ^. moduleNotInScopeName)
          msg = "The module" <+> ppCode opts' _moduleNotInScopeName <+> "is not in scope"

newtype UnusedOperatorDef = UnusedOperatorDef
  { _unusedOperatorDef :: OperatorSyntaxDef
  }
  deriving stock (Show)

instance ToGenericError UnusedOperatorDef where
  genericError UnusedOperatorDef {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _unusedOperatorDef
          msg =
            "Unused operator syntax definition:"
              <> line
              <> ppCode opts' _unusedOperatorDef

data AmbiguousSym = AmbiguousSym
  { _ambiguousSymName :: Name,
    _ambiguousSymEntires :: [SymbolEntry]
  }
  deriving stock (Show)

instance ToGenericError AmbiguousSym where
  genericError AmbiguousSym {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = i : is
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _ambiguousSymName
          is = map getLoc _ambiguousSymEntires
          msg = ambiguousMessage opts' _ambiguousSymName _ambiguousSymEntires

data AmbiguousModuleSym = AmbiguousModuleSym
  { _ambiguousModName :: Name,
    _ambiguousModSymEntires :: [SymbolEntry]
  }
  deriving stock (Show)

instance ToGenericError AmbiguousModuleSym where
  genericError AmbiguousModuleSym {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = i : is
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _ambiguousModName
          is = map getLoc _ambiguousModSymEntires
          msg = ambiguousMessage opts' _ambiguousModName _ambiguousModSymEntires

data WrongLocationCompileBlock = WrongLocationCompileBlock
  { _wrongLocationCompileBlockExpectedModPath :: S.AbsModulePath,
    _wrongLocationCompileBlockName :: Name
  }
  deriving stock (Show)

instance ToGenericError WrongLocationCompileBlock where
  genericError WrongLocationCompileBlock {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          name = _wrongLocationCompileBlockName
          i = getLoc name
          msg =
            "The compilation rules for the symbol"
              <+> ppCode opts' name
              <+> "need to be defined in the module:"
                <> line
                <> ppCode opts' _wrongLocationCompileBlockExpectedModPath

data MultipleCompileBlockSameName = MultipleCompileBlockSameName
  { _multipleCompileBlockFirstDefined :: Interval,
    _multipleCompileBlockSym :: Symbol
  }
  deriving stock (Show)

instance ToGenericError MultipleCompileBlockSameName where
  genericError MultipleCompileBlockSameName {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i2,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i1, i2]
            }
        where
          opts' = fromGenericOptions opts
          i1 = _multipleCompileBlockFirstDefined
          i2 = getLoc _multipleCompileBlockSym
          msg =
            "Multiple compile blocks for the symbol"
              <+> ppCode opts' _multipleCompileBlockSym

data MultipleCompileRuleSameBackend = MultipleCompileRuleSameBackend
  { _multipleCompileRuleSameBackendBackendItem :: BackendItem,
    _multipleCompileRuleSameBackendSym :: Symbol
  }
  deriving stock (Show)

instance ToGenericError MultipleCompileRuleSameBackend where
  genericError MultipleCompileRuleSameBackend {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          backend = _multipleCompileRuleSameBackendBackendItem ^. backendItemBackend
          name = _multipleCompileRuleSameBackendSym
          i = getLoc _multipleCompileRuleSameBackendSym
          msg =
            "Multiple"
              <+> ppCode opts' backend
              <+> "compilation rules for the symbol"
              <+> ppCode opts' name
              <+> "at"
              <+> pretty (getLoc name)

data WrongKindExpressionCompileBlock = WrongKindExpressionCompileBlock
  { _wrongKindExpressionCompileBlockSym :: Symbol,
    _wrongKindExpressionCompileBlockEntry :: SymbolEntry
  }
  deriving stock (Show)

instance ToGenericError WrongKindExpressionCompileBlock where
  genericError WrongKindExpressionCompileBlock {..} = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          i = getLoc _wrongKindExpressionCompileBlockSym
          msg =
            "Symbol"
              <+> ppCode opts' _wrongKindExpressionCompileBlockSym
              <+> "is not a constructor, inductive data type, axiom nor a function."
                <> "Thus, it cannot have a compile rule."

infixErrorAux :: Doc Ann -> Doc Ann -> Doc Ann
infixErrorAux kind pp =
  "Error while resolving infixities in the"
    <+> kind
      <> ":"
      <> line
      <> indent' pp

ambiguousMessage :: Options -> Name -> [SymbolEntry] -> Doc Ann
ambiguousMessage opts' n es =
  "The symbol"
    <+> ppCode opts' n
    <+> "at"
    <+> pretty (getLoc n)
    <+> "is ambiguous."
      <> line
      <> "It could be any of:"
      <> line
      <> indent' (vsep (map (ppCode opts') es))

newtype DuplicateInductiveParameterName = DuplicateInductiveParameterName
  { _duplicateInductiveParameterName :: Symbol
  }
  deriving stock (Show)

makeLenses ''DuplicateInductiveParameterName

instance ToGenericError DuplicateInductiveParameterName where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          param = e ^. duplicateInductiveParameterName
          i = getLoc param
          msg =
            "Invalid name"
              <+> ppCode opts' param
                <> "."
                <> line
                <> "Inductive parameter names can not be repeated."

newtype DoubleBracesPattern = DoubleBracesPattern
  { _doubleBracesPatternArg :: PatternArg
  }
  deriving stock (Show)

makeLenses ''DoubleBracesPattern

instance ToGenericError DoubleBracesPattern where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          pat :: PatternArg
          pat = e ^. doubleBracesPatternArg
          i = getLoc pat
          msg =
            "Double braces are not valid:"
              <+> code (braces (ppCode opts' pat))

data DoubleBinderPattern = DoubleBinderPattern
  { _doubleBinderPatternName :: S.Symbol,
    _doubleBinderPatternArg :: PatternArg
  }
  deriving stock (Show)

makeLenses ''DoubleBinderPattern

instance ToGenericError DoubleBinderPattern where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          name = e ^. doubleBinderPatternName
          pat = e ^. doubleBinderPatternArg
          i = getLoc pat
          msg =
            "As-Patterns cannot be nested:"
              <+> code (ppCode opts' name <> kwAt <> parens (ppCode opts' pat))

newtype AliasBinderPattern = AliasBinderPattern
  { _aliasBinderPatternArg :: PatternArg
  }
  deriving stock (Show)

makeLenses ''AliasBinderPattern

instance ToGenericError AliasBinderPattern where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          pat = e ^. aliasBinderPatternArg
          i = getLoc pat
          msg =
            "As-Patterns cannot be used to alias pattern variables:"
              <+> code (ppCode opts' pat)

newtype ImplicitPatternLeftApplication = ImplicitPatternLeftApplication
  { _implicitPatternLeftApplication :: PatternApp
  }
  deriving stock (Show)

makeLenses ''ImplicitPatternLeftApplication

instance ToGenericError ImplicitPatternLeftApplication where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          pat :: PatternApp
          pat = e ^. implicitPatternLeftApplication
          i = getLoc pat
          msg =
            "Pattern matching an implicit argument cannot occur on the left of an application:"
              <+> ppCode opts' pat

newtype ConstructorExpectedLeftApplication = ConstructorExpectedLeftApplication
  { _constructorExpectedLeftApplicationPattern :: Pattern
  }
  deriving stock (Show)

makeLenses ''ConstructorExpectedLeftApplication

instance ToGenericError ConstructorExpectedLeftApplication where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = prettyError msg,
              _genericErrorIntervals = [i]
            }
        where
          opts' = fromGenericOptions opts
          pat :: Pattern
          pat = e ^. constructorExpectedLeftApplicationPattern
          i = getLoc pat
          msg =
            "Constructor expected on the left of a pattern application:"
              <+> ppCode opts' pat
