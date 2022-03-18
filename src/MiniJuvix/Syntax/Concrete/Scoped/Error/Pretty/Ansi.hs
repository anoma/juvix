module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Ansi where

import Prettyprinter
import MiniJuvix.Prelude
import Prettyprinter.Render.Terminal
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> colorDull Red
