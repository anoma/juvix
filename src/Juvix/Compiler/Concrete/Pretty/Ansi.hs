module Juvix.Compiler.Concrete.Pretty.Ansi where

import Juvix.Compiler.Concrete.Data
import Juvix.Compiler.Concrete.Pretty.Base
import Juvix.Prelude
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnDelimiter -> colorDull White
  AnnComment -> colorDull Cyan
  AnnKeyword -> colorDull Blue
  AnnDef {} -> mempty
  AnnRef {} -> mempty
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
  AnnUnkindedSym -> mempty
