module Juvix.Compiler.Abstract.Pretty.Ansi
  ( module Juvix.Compiler.Abstract.Pretty.Base,
    module Juvix.Compiler.Abstract.Pretty.Ansi,
  )
where

import Juvix.Compiler.Abstract.Pretty.Base
import Juvix.Compiler.Concrete.Data.NameKind
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnImportant -> bold
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
