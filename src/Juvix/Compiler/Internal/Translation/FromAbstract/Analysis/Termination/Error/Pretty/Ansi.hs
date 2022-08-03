module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ansi
  ( module Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ansi,
  )
where

import Juvix.Compiler.Abstract.Pretty.Ansi qualified as M
import Juvix.Compiler.Internal.Translation.FromAbstract.Analysis.Termination.Error.Pretty.Ann
import Juvix.Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> bold
  AbstractAnn m -> M.stylize m
