module MiniJuvix.Syntax.MiniHaskell.Pretty.Ansi where

import MiniJuvix.Syntax.MiniHaskell.Language
import MiniJuvix.Syntax.MiniHaskell.Pretty.Ann
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
