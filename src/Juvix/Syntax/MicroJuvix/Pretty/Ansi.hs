module Juvix.Syntax.MicroJuvix.Pretty.Ansi where

import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.Pretty.Base
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
