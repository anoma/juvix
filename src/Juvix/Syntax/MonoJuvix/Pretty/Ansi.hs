module Juvix.Syntax.MonoJuvix.Pretty.Ansi where

import Juvix.Prelude
import Juvix.Syntax.MonoJuvix.Language
import Juvix.Syntax.MonoJuvix.Pretty.Ann
import Juvix.Syntax.MonoJuvix.Pretty.Base
import Prettyprinter
import Prettyprinter.Render.Terminal

printPrettyCodeDefault :: PrettyCode c => c -> IO ()
printPrettyCodeDefault = printPrettyCode defaultOptions

printPrettyCode :: PrettyCode c => Options -> c -> IO ()
printPrettyCode = hPrintPrettyCode stdout

hPrintPrettyCode :: PrettyCode c => Handle -> Options -> c -> IO ()
hPrintPrettyCode h opts = renderIO h . docStream' opts

renderPrettyCode :: PrettyCode c => Options -> c -> Text
renderPrettyCode opts = renderStrict . docStream' opts

docStream' :: PrettyCode c => Options -> c -> SimpleDocStream AnsiStyle
docStream' opts =
  reAnnotateS stylize . docStream opts

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
