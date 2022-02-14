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
    ints = map ppInterval [_symbolDefined _multipleDeclEntry, _multipleDeclSecond]

instance PrettyError InfixError where
  ppError InfixError {..} =
    "Error while resolving infixities in the expression:" <> line
    <> indent' (highlight $ ppCode _infixErrAtoms)
