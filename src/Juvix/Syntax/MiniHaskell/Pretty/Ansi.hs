module Juvix.Syntax.MiniHaskell.Pretty.Ansi where

import Juvix.Syntax.MiniHaskell.Language
import Juvix.Syntax.MiniHaskell.Pretty.Ann
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
