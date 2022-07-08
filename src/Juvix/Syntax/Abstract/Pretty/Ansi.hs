module Juvix.Syntax.Abstract.Pretty.Ansi
  ( module Juvix.Syntax.Abstract.Pretty.Base,
    module Juvix.Syntax.Abstract.Pretty.Ansi,
  )
where

import Juvix.Syntax.Abstract.Pretty.Base
import Juvix.Syntax.Concrete.Scoped.Name.NameKind
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnImportant -> bold
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
