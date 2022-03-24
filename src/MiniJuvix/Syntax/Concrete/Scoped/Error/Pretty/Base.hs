module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty.Extra as NonEmpty
import MiniJuvix.Prelude
import qualified MiniJuvix.Syntax.Concrete.Language as L
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as P
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import Prettyprinter
import Text.EditDistance

data Eann = Highlight
  | ScopedAnn P.Ann

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

class PrettyError e where
  ppError :: e -> Doc Eann

instance PrettyError MultipleDeclarations where
  ppError MultipleDeclarations {..} =
    "Multiple declarations of" <+> ppSymbolT _multipleDeclSymbol <> line
      <> "Declared at:" <+> align (vsep ints)
    where
      ints = map pretty [S._nameDefined (L.symbolEntryToSName _multipleDeclEntry), _multipleDeclSecond]

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

instance PrettyError LacksFunctionClause where
  ppError LacksFunctionClause {..} =
    pretty loc <> line
      <> "There is a type signature with no function clause:"
      <> line
      <> indent' (highlight (ppCode _lacksFunctionClause))
    where
      loc = getLoc $ _sigName _lacksFunctionClause

instance PrettyError LacksTypeSig where
  ppError LacksTypeSig {..} =
    pretty loc <> line
      <> "There is a declaration with a missing type signature:"
      <> line
      <> indent' (highlight (ppCode _lacksTypeSigClause))
    where
      loc = getLoc $ _clauseOwnerFunction _lacksTypeSigClause

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

instance PrettyError NotInScope where
  ppError NotInScope {..} =
    pretty loc <> line
      <> "Symbol not in scope:" <+> highlight (ppCode _notInScopeSymbol)
      <?> ((line <>) <$> suggestion)
    where
      suggestion
        | null suggestions = Nothing
        | otherwise = Just $ "Perhaps you meant:" <+> align (vsep suggestions)
      loc = getLoc _notInScopeSymbol
      sym = _symbolText _notInScopeSymbol
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
        HashSet.fromList (map _symbolText (HashMap.keys $ _localVars _notInScopeLocal))
          <> HashSet.fromList (map _symbolText (HashMap.keys $ _scopeSymbols _notInScopeScope))

instance PrettyError BindGroupConflict where
  ppError BindGroupConflict {..} =
    "The symbol" <+> highlight (ppCode _bindGroupFirst)
      <+> "appears twice in the same binding group:"
      <> line
      <> indent' (align locs)
    where
      locs = vsep $ map (pretty . getLoc) [_bindGroupFirst, _bindGroupSecond]

instance PrettyError DuplicateFixity where
  ppError DuplicateFixity {..} =
    "Multiple fixity declarations for symbol" <+> highlight (ppCode sym) <> ":" <> line
      <> indent' (align locs)
    where
      sym = opSymbol _dupFixityFirst
      locs = vsep $ map (pretty . getLoc) [_dupFixityFirst, _dupFixityFirst]

instance PrettyError MultipleExportConflict where
  ppError MultipleExportConflict {..} =
    "The symbol" <+> highlight (ppCode _multipleExportSymbol) <+> "is exported multiple times in the module"
      <+> ppCode _multipleExportModule

instance PrettyError ModuleNotInScope where
  ppError ModuleNotInScope {..} =
    "The module" <+> ppCode _moduleNotInScopeName <+> "is not in scope"

instance PrettyError MegaParsecError where
  ppError MegaParsecError {..} = pretty _megaParsecError

instance PrettyError WrongTopModuleName where
  ppError WrongTopModuleName {..} =
    "The top module" <+> ppCode _wrongTopModuleNameActualName <+> "is defined in the file:" <> line
      <> highlight (pretty _wrongTopModuleNameActualPath)
      <> line
      <> "But it should be in the file:"
      <> line
      <> pretty _wrongTopModuleNameExpectedPath

instance PrettyError UnusedOperatorDef where
  ppError UnusedOperatorDef {..} =
    "Unused operator syntax definition:" <> line
      <> ppCode _unusedOperatorDef

instance PrettyError AmbiguousSym where
  ppError AmbiguousSym {..} =
    "The symbol" <+> ppCode _ambiguousSymName <+> "at" <+> pretty (getLoc _ambiguousSymName) <+> "is ambiguous." <> line
      <> "It could be any of:"
      <> line
      <> indent' (vsep (map ppCode _ambiguousSymEntires))

instance PrettyError AmbiguousModuleSym where
  ppError AmbiguousModuleSym {} =
    "TODO Ambiguous module symbol"
