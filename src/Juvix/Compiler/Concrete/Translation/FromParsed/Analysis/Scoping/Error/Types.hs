module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types
  ( module Juvix.Compiler.Concrete.Language,
    module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Types,
    module Juvix.Data.CodeAnn,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Concrete.Data.Scope
import Juvix.Compiler.Concrete.Data.ScopedName qualified as S
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Pretty.Options (Options, fromGenericOptions)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty
import Juvix.Data.CodeAnn
import Juvix.Prelude

data MultipleDeclarations = MultipleDeclarations
  { _multipleDeclFirst :: Interval,
    _multipleDeclSecond :: Symbol
  }
  deriving stock (Show)

instance ToGenericError MultipleDeclarations where
  genericError MultipleDeclarations {..} =
    return
      GenericError
        { _genericErrorLoc = i2,
          _genericErrorMessage = prettyError msg,
          _genericErrorIntervals = [i1, i2]
        }
    where
      i1 :: Interval
      i1 = _multipleDeclFirst
      i2 :: Interval
      i2 = getLoc _multipleDeclSecond
      msg =
        "Multiple declarations of"
          <+> pretty _multipleDeclSecond
            <> line
            <> "Declared at:"
            <> line
            <> itemize (map pretty [i1, i2])

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
              <> checkColon _lacksTypeSigClause

          checkColon :: FunctionClause 'Parsed -> Doc Ann
          checkColon fc@FunctionClause {..} =
            case Text.splitOn ":" (_clauseOwnerFunction ^. withLocParam) of
              [x, ""] -> makeMessage x [":"]
              [x, y] -> makeMessage x [":", y]
              _ -> mempty
            where
              makeMessage :: Text -> [Text] -> Doc Ann
              makeMessage x xs =
                line
                  <> "Perhaps you meant:"
                  <> line
                  <> indent'
                    ( ppCode
                        opts'
                        (adjustPatterns x xs)
                    )

              adjustPatterns :: Text -> [Text] -> FunctionClause 'Parsed
              adjustPatterns x xs =
                ( over clauseOwnerFunction (set withLocParam x) $
                    over clausePatterns (map mkpat xs ++) fc
                )

              mkpat :: Text -> PatternAtom 'Parsed
              mkpat txt = PatternAtomIden (NameUnqualified (set withLocParam txt _clauseOwnerFunction))

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

data DuplicateIterator = DuplicateIterator
  { _dupIteratorFirst :: IteratorSyntaxDef,
    _dupIteratorSecond :: IteratorSyntaxDef
  }
  deriving stock (Show)

instance ToGenericError DuplicateIterator where
  genericError DuplicateIterator {..} = ask >>= generr
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
          i1 = getLoc _dupIteratorFirst
          i2 = getLoc _dupIteratorSecond

          msg =
            "Multiple iterator declarations for symbol"
              <+> ppCode opts' sym
                <> ":"
                <> line
                <> indent' (align locs)
            where
              sym = _dupIteratorFirst ^. iterSymbol
              locs = vsep $ map (pretty . getLoc) [_dupIteratorFirst, _dupIteratorFirst]

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
            HashSet.fromList (map (^. symbolText) (HashMap.keys (_notInScopeScope ^. scopeSymbols)))

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

newtype UnusedIteratorDef = UnusedIteratorDef
  { _unusedIteratorDef :: IteratorSyntaxDef
  }
  deriving stock (Show)

instance ToGenericError UnusedIteratorDef where
  genericError UnusedIteratorDef {..} = ask >>= generr
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
          i = getLoc _unusedIteratorDef
          msg =
            "Unused iterator syntax definition:"
              <> line
              <> ppCode opts' _unusedIteratorDef

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
      <> itemize (map (ppMessage opts') es)

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

newtype CaseBranchImplicitPattern = CaseBranchImplicitPattern
  { _caseBranchImplicitPattern :: PatternArg
  }
  deriving stock (Show)

instance ToGenericError CaseBranchImplicitPattern where
  genericError :: Member (Reader GenericOptions) r => CaseBranchImplicitPattern -> Sem r GenericError
  genericError CaseBranchImplicitPattern {..} = do
    opts <- fromGenericOptions <$> ask
    let msg = "The pattern" <+> ppCode opts _caseBranchImplicitPattern <+> "is not valid because implicit patterns are not allowed in case branches"
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _caseBranchImplicitPattern

data ModuleDoesNotExportSymbol = ModuleDoesNotExportSymbol
  { _moduleDoesNotExportSymbol :: Symbol,
    _moduleDoesNotExportModule :: ModuleRef
  }
  deriving stock (Show)

instance ToGenericError ModuleDoesNotExportSymbol where
  genericError :: Member (Reader GenericOptions) r => ModuleDoesNotExportSymbol -> Sem r GenericError
  genericError ModuleDoesNotExportSymbol {..} = do
    opts <- fromGenericOptions <$> ask
    let msg =
          "The module"
            <+> ppCode opts _moduleDoesNotExportModule
            <+> "does not export"
            <+> ppCode opts _moduleDoesNotExportSymbol
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _moduleDoesNotExportSymbol

newtype IteratorInitializer = IteratorInitializer
  { _iteratorInitializerIterator :: Iterator 'Parsed
  }
  deriving stock (Show)

instance ToGenericError IteratorInitializer where
  genericError IteratorInitializer {..} = do
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText ("Wrong number of iterator initializers" :: Doc Ann),
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _iteratorInitializerIterator

newtype IteratorRange = IteratorRange
  { _iteratorRangeIterator :: Iterator 'Parsed
  }
  deriving stock (Show)

instance ToGenericError IteratorRange where
  genericError IteratorRange {..} = do
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText ("Wrong number of iterator ranges" :: Doc Ann),
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _iteratorRangeIterator

newtype IteratorUndefined = IteratorUndefined
  { _iteratorUndefinedIterator :: Iterator 'Parsed
  }
  deriving stock (Show)

instance ToGenericError IteratorUndefined where
  genericError IteratorUndefined {..} = do
    opts <- fromGenericOptions <$> ask
    let msg =
          "The identifier"
            <+> ppCode opts (_iteratorUndefinedIterator ^. iteratorName)
            <+> "has not been declared as an iterator"
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText msg,
          _genericErrorIntervals = [i]
        }
    where
      i :: Interval
      i = getLoc _iteratorUndefinedIterator
