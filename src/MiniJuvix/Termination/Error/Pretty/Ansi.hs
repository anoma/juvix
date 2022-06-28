module MiniJuvix.Termination.Error.Pretty.Ansi
  ( module MiniJuvix.Termination.Error.Pretty.Ansi,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Pretty.Ansi qualified as M
import MiniJuvix.Termination.Error.Pretty.Ann
import Prettyprinter
import Prettyprinter.Render.Terminal

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> bold
  AbstractAnn m -> M.stylize m
