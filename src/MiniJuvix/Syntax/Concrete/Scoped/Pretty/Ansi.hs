module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnDelimiter -> colorDull White
  AnnKeyword -> colorDull Blue
  AnnDef {} -> mempty
  AnnRef {} -> mempty
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
  AnnUnkindedSym -> mempty
