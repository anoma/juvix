module MiniJuvix.Syntax.Concrete.Scoped.Pretty.Ansi where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Concrete.Scoped.Pretty.Base
import Prettyprinter
import Prettyprinter.Render.Terminal
import MiniJuvix.Prelude.Pretty

printPrettyCodeDefault :: PrettyCode c => c -> IO ()
printPrettyCodeDefault = printPrettyCode defaultOptions

printPrettyCode :: PrettyCode c => Options -> c -> IO ()
printPrettyCode = hPrintPrettyCode stdout

hPrintPrettyCode :: PrettyCode c => Handle -> Options -> c -> IO ()
hPrintPrettyCode h opts = renderIO h . docStream opts

renderPrettyCode :: PrettyCode c => Options -> c -> Text
renderPrettyCode opts = renderStrict . docStream opts

docStream :: PrettyCode c => Options -> c -> SimpleDocStream AnsiStyle
docStream opts =
  reAnnotateS stylize . layoutPretty defaultLayoutOptions
    . run
    . runReader opts
    . ppCode

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnDelimiter -> colorDull White
  AnnKeyword -> colorDull Blue
  AnnDef {} -> mempty
  AnnRef {} -> mempty
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
  AnnUnkindedSym -> mempty

newtype PPOutput = PPOutput (SimpleDocStream Ann)

instance HasAnsiBackend PPOutput where
  toAnsi (PPOutput o) = reAnnotateS stylize o

instance HasTextBackend PPOutput where
  toText (PPOutput o) = unAnnotateS o
