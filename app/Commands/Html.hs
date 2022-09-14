module Commands.Html where

import Commands.Base
import Commands.Html.Options
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Html
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper

runCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runCommand HtmlOptions {..} = do
  res <- runPipeline _htmlInputFile upToScoping
  let m = head (res ^. Scoper.resultModules)
  embed (Html.genHtml Concrete.defaultOptions _htmlRecursive _htmlTheme (_htmlOutputDir ^. pathPath) _htmlPrintMetadata m)
