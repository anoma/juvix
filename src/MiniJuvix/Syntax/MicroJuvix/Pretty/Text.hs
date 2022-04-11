module MiniJuvix.Syntax.MicroJuvix.Pretty.Text where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Pretty.Base
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
