module MiniJuvix.Syntax.Pretty where

import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi qualified as ScopedAnsi
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Html
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Text qualified as ScopedText

prettyScoper :: PrettyCode c => Bool -> Options -> c -> IO ()
prettyScoper = \case
  True -> ScopedAnsi.printPrettyCode
  _ -> ScopedText.printPrettyCode
