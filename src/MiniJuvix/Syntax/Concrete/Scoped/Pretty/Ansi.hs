module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi where

import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name (NameKind (..))
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Utils.Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal

printTopModuleDefault :: Module 'Scoped 'ModuleTop -> IO ()
printTopModuleDefault = printTopModule defaultOptions

printTopModule :: Options -> Module 'Scoped 'ModuleTop -> IO ()
printTopModule opts m = renderIO stdout docStream'
  where
    docStream :: SimpleDocStream Ann
    docStream = layoutPretty defaultLayoutOptions (prettyTopModule opts m)
    docStream' :: SimpleDocStream AnsiStyle
    docStream' = reAnnotateS stylize docStream

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> case k of
    KNameConstructor -> colorDull Magenta
    KNameInductive -> colorDull Green
    KNameAxiom -> colorDull Red
    KNameLocalModule -> mempty
    KNameFunction -> colorDull Yellow
    KNameLocal -> mempty
    KNameTopModule -> mempty
  AnnDelimiter -> colorDull White
  AnnKeyword -> colorDull Blue
  AnnDef {} -> mempty
  AnnRef {} -> mempty
  AnnNumber -> mempty
  AnnUnkindedSym -> mempty
