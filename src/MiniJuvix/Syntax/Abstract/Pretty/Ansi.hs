module MiniJuvix.Syntax.Abstract.Pretty.Ansi
  ( module MiniJuvix.Syntax.Abstract.Pretty.Base,
    module MiniJuvix.Syntax.Abstract.Pretty.Ansi,
  )
where

import MiniJuvix.Syntax.Abstract.Pretty.Base
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import Prettyprinter.Render.Terminal

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnImportant -> bold
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
