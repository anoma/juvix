-- TODO: MOVE
module Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Pretty.Ansi where

import Juvix.Compiler.Concrete.Pretty.Ansi qualified as S
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping.Error.Ann
import Prettyprinter.Render.Terminal

stylize :: Eann -> AnsiStyle
stylize a = case a of
  Highlight -> bold
  ScopedAnn s -> S.stylize s
