module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base where

import Prettyprinter
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as P
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S
import Text.EditDistance
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

data Eann = Highlight

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

ppSymbolT :: Text -> Doc Eann
ppSymbolT = highlight . pretty

ppCode :: P.PrettyCode c => c -> Doc Eann
ppCode = unAnnotate . P.runPrettyCode P.defaultOptions

indent' :: Doc ann -> Doc ann
indent' = indent 2

textDistance :: Text -> Text -> Int
textDistance a b =
  restrictedDamerauLevenshteinDistance defaultEditCosts
  (unpack a) (unpack b)

class PrettyError e where
  ppError :: e -> Doc Eann

instance PrettyError MultipleDeclarations where
  ppError MultipleDeclarations {..} =
    "Multiple declarations of" <+> ppSymbolT _multipleDeclSymbol <> line
    <> "Declared at:" <+> align (vsep ints)
    where
    ints = map pretty [_symbolDefined _multipleDeclEntry, _multipleDeclSecond]

instance PrettyError InfixError where
  ppError InfixError {..} =
    infixErrorAux "expression" (ppCode _infixErrAtoms)

instance PrettyError InfixErrorP where
  ppError InfixErrorP {..} = infixErrorAux "pattern" (ppCode _infixErrAtomsP)

infixErrorAux :: Doc Eann -> Doc Eann -> Doc Eann
infixErrorAux kind pp =
  "Error while resolving infixities in the" <+> kind <> ":" <> line
    <> indent' (highlight pp)

instance PrettyError LacksTypeSig where
  ppError LacksTypeSig {..} =
     pretty loc <> line <>
     "Missing type signature of declaration:" <> line
     <> indent' (highlight (ppCode _lacksTypeSigClause))
    where
    loc = _symbolLoc $ clauseOwnerFunction _lacksTypeSigClause

instance PrettyError ImportCycle where
  ppError ImportCycle {..} =
    pretty loc <> line <>
    "The following import causes an import cycle:" <> line
    <> indent' (highlight (ppCode _importCycleImport))
    where
    loc :: Interval
    loc = _symbolLoc $ modulePathName $ importModule _importCycleImport

instance PrettyError NotInScope where
  ppError NotInScope {..} =
    pretty loc <> line <>
    "Symbol not in scope:" <+> highlight (ppCode _notInScopeSymbol) <> line <>
    "Perhaps you meant:" <+> align (vsep suggestions)
    where
    loc = _symbolLoc _notInScopeSymbol
    sym = _symbolText _notInScopeSymbol
    maxDist :: Int
    maxDist = 2
    suggestions :: [Doc a]
    suggestions =
        map (pretty . fst) $
        sortOn snd
        [ (c, dist)  | c <- toList candidates
          , let dist =  textDistance sym c, dist <= maxDist ]
    candidates :: HashSet Text
    candidates = HashSet.fromList (map _symbolText (HashMap.keys $ _localVars _notInScopeLocal)) <>
      HashSet.fromList (map _symbolText (HashMap.keys $ _scopeSymbols _notInScopeScope))
