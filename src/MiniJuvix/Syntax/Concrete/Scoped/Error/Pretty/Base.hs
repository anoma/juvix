module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base where

import Prettyprinter
import MiniJuvix.Utils.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types
import MiniJuvix.Syntax.Concrete.Scoped.Scope
import qualified MiniJuvix.Syntax.Concrete.Scoped.Name as S

data Eann = Highlight

ppSymbolT :: Text -> Doc Eann
ppSymbolT = squotes . pretty

ppMultipleDeclarations :: MultipleDeclarations -> Doc Eann
ppMultipleDeclarations MultipleDeclarations {..} =
  "Multiple declarations of" <+> ppSymbolT _multipleDeclSymbol <> line
  <> "Declared at:" <+> align (vsep ints)
  where
  ints = map ppInterval [_symbolDefined _multipleDeclEntry, _multipleDeclSecond]

ppScopeError :: ScopeError -> Doc Eann
ppScopeError e = case e of
  ErrParser txt -> pretty txt
  ErrGeneric txt -> pretty txt
  ErrInfixParser txt -> pretty txt
  ErrInfixPattern txt -> pretty txt
  ErrMultipleDeclarations er -> ppMultipleDeclarations er
  ErrLacksTypeSig {} -> ugly
  ErrImportCycle {} -> ugly
  ErrOpenNotInScope {} -> ugly
  ErrSymNotInScope {} -> ugly
  ErrQualSymNotInScope {} -> ugly
  ErrModuleNotInScope {} -> ugly
  ErrBindGroup {} -> ugly
  ErrDuplicateFixity {} -> ugly
  ErrMultipleExport {} -> ugly
  ErrAmbiguousSym {} -> ugly
  ErrAmbiguousModuleSym {} -> ugly
  where
  ugly = pretty (show e :: Text)
