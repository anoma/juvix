module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base where

import Prettyprinter
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base as P
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

data Eann = Highlight

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

ppSymbolT :: Text -> Doc Eann
ppSymbolT = squotes . highlight . pretty

ppCode :: P.PrettyCode c => c -> Doc Eann
ppCode = unAnnotate . P.runPrettyCode P.defaultOptions

indent' :: Doc ann -> Doc ann
indent' = indent 2

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
