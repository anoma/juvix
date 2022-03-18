module MiniJuvix.Syntax.Abstract.Pretty.Ansi (
module MiniJuvix.Syntax.Abstract.Pretty.Base,
module MiniJuvix.Syntax.Abstract.Pretty.Ansi
                                             ) where

import MiniJuvix.Syntax.Concrete.Scoped.Name.NameKind
import MiniJuvix.Syntax.Abstract.Pretty.Base
import MiniJuvix.Prelude
import Prettyprinter
import Prettyprinter.Render.Terminal

printPrettyCodeDefault :: PrettyCode c => c -> IO ()
printPrettyCodeDefault = printPrettyCode defaultOptions

printPrettyCode :: PrettyCode c => Options -> c -> IO ()
printPrettyCode = hPrintPrettyCode stdout

hPrintPrettyCode :: PrettyCode c => Handle -> Options -> c -> IO ()
hPrintPrettyCode h opts = renderIO h . docStream opts

renderPrettyCode :: PrettyCode c => Options -> c -> Text
renderPrettyCode opts = renderStrict . docStream opts

docStream :: PrettyCode c => Options -> c -> SimpleDocStream AnsiStyle
docStream opts = reAnnotateS stylize . layoutPretty defaultLayoutOptions
    . run . runReader opts . ppCode

stylize :: Ann -> AnsiStyle
stylize a = case a of
  AnnKind k -> nameKindAnsi k
  AnnKeyword -> colorDull Blue
  AnnImportant -> bold
  AnnLiteralString -> colorDull Red
  AnnLiteralInteger -> colorDull Cyan
