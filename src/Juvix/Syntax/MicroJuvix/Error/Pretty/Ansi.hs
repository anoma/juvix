module Juvix.Syntax.MicroJuvix.Error.Pretty.Ansi where

import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Error.Pretty.Ann
import Juvix.Syntax.MicroJuvix.Pretty.Ansi qualified as M
import Prettyprinter
import Prettyprinter.Render.Terminal

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> bold
  MicroAnn m -> M.stylize m
