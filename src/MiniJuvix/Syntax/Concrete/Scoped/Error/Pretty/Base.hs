module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base where

import Prettyprinter
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

data Eann = Highlight

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight

ppSymbolT :: Text -> Doc Eann
ppSymbolT = squotes . highlight . pretty

ppMultipleDeclarations :: MultipleDeclarations -> Doc Eann
ppMultipleDeclarations MultipleDeclarations {..} =
  "Multiple declarations of" <+> ppSymbolT _multipleDeclSymbol <> line
  <> "Declared at:" <+> align (vsep ints)
  where
  ints = map ppInterval [_symbolDefined _multipleDeclEntry, _multipleDeclSecond]
