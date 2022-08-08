module Juvix.Compiler.Core.Pretty.Ansi where

import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty.Base
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
