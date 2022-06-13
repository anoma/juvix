module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ansi where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Pretty.Ansi qualified as M
import Prettyprinter
import Prettyprinter.Render.Terminal

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> bold
  MicroAnn m -> M.stylize m
