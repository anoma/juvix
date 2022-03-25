module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Text where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import Prettyprinter
import Prettyprinter.Render.Text

printPrettyCodeDefault :: PrettyCode c => c -> IO ()
printPrettyCodeDefault = printPrettyCode defaultOptions

printPrettyCode :: PrettyCode c => Options -> c -> IO ()
printPrettyCode = hPrintPrettyCode stdout

hPrintPrettyCode :: PrettyCode c => Handle -> Options -> c -> IO ()
hPrintPrettyCode h opts = renderIO h . docStream opts

renderPrettyCodeDefault :: PrettyCode c => c -> Text
renderPrettyCodeDefault = renderStrict . docStream defaultOptions

renderPrettyCode :: PrettyCode c => Options -> c -> Text
renderPrettyCode opts = renderStrict . docStream opts

docStream :: PrettyCode c => Options -> c -> SimpleDocStream Ann
docStream opts =
  layoutPretty defaultLayoutOptions
    . run
    . runReader opts
    . ppCode
