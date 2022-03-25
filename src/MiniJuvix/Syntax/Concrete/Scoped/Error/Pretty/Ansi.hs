module MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Ansi where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Error.Pretty.Base
import qualified MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi as S

import Prettyprinter
import Prettyprinter.Render.Terminal

renderAnsi :: SimpleDocStream Eann -> Text
renderAnsi = renderStrict . reAnnotateS stylize

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> colorDull Red
  ScopedAnn s -> S.stylize s
