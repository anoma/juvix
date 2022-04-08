module MiniJuvix.Syntax.Concrete.Scoped.Pretty where

------------------------------------------------------------------------------

import MiniJuvix.Prelude.Pretty
import MiniJuvix.Prelude.Pretty qualified as Pretty
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ann
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import MiniJuvix.Prelude
import Prettyprinter.Render.Text qualified as Text
import Prettyprinter.Render.Terminal qualified as Ansi

------------------------------------------------------------------------------

newtype PPOutput = PPOutput (SimpleDocStream Ann)

printPrettyCodeDefault :: PrettyCode c => Bool -> c -> IO ()
printPrettyCodeDefault b = printPrettyCode b defaultOptions

printPrettyCode :: PrettyCode c => Bool -> Options -> c -> IO ()
printPrettyCode b = hPrintPrettyCode b stdout

hPrintPrettyCode :: PrettyCode c => Bool -> Handle -> Options -> c -> IO ()
hPrintPrettyCode b h opts = hRenderIO b h . PPOutput . docStream opts

renderPrettyCode :: PrettyCode c => Bool -> Options -> c -> Text
renderPrettyCode b opts = toAnsiText b . PPOutput . docStream opts

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS Ansi.stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
