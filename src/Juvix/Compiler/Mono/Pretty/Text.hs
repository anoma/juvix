module Juvix.Compiler.Mono.Pretty.Text where

import Juvix.Compiler.Mono.Pretty.Base
import Juvix.Prelude
import Prettyprinter.Render.Text as Text

printPrettyCodeDefault :: PrettyCode c => c -> IO ()
printPrettyCodeDefault = printPrettyCode defaultOptions

printPrettyCode :: PrettyCode c => Options -> c -> IO ()
printPrettyCode = hPrintPrettyCode stdout

hPrintPrettyCode :: PrettyCode c => Handle -> Options -> c -> IO ()
hPrintPrettyCode h opts = Text.renderIO h . docStream opts

renderPrettyCodeDefault :: PrettyCode c => c -> Text
renderPrettyCodeDefault = renderStrict . docStream defaultOptions

renderPrettyCode :: PrettyCode c => Options -> c -> Text
renderPrettyCode opts = renderStrict . docStream opts
