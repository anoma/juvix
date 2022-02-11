module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Ansi where

import Prettyprinter
import MiniJuvix.Utils.Prelude
import Prettyprinter.Render.Terminal
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base
import MiniJuvix.Syntax.Concrete.Scoped.Error.Types

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> mempty
