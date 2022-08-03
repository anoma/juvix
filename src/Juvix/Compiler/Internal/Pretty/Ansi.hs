module Juvix.Compiler.Internal.Pretty.Ansi where

import Juvix.Compiler.Internal.Language
import Juvix.Compiler.Internal.Pretty.Base
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
