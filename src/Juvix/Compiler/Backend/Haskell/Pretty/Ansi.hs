module Juvix.Compiler.Backend.Haskell.Pretty.Ansi where

import Juvix.Compiler.Backend.Haskell.Language
import Juvix.Compiler.Backend.Haskell.Pretty.Ann
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
