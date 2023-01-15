module Commands.Html where

import Commands.Base
import Commands.Html.Options
import Juvix.Compiler.Backend.Html.Data.Options qualified as Html
import Juvix.Compiler.Backend.Html.Translation.FromTyped (JudocArgs (..), PlainHtmlArgs (..))
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Html
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Extra.Process
import System.Process qualified as Process

runSourceHtmlCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runSourceHtmlCommand HtmlOptions {..} = do
  res <- runPipeline _htmlInputFile upToScoping
  let m = head (res ^. Scoper.resultModules)
  outputDir <- someBaseToAbs' (_htmlOutputDir ^. pathPath)
  let htmlOpts = Html.HtmlOptions {
      _htmlOptionsOutputDir = outputDir
    }
  embed $
    Html.genPlainHtml htmlOpts
          PlainHtmlArgs
            { _plainHtmlArgsConcreteOpts = Concrete.defaultOptions,
              _plainHtmlArgsRecursive = _htmlRecursive,
              _plainHtmlArgsTheme = _htmlTheme,
              _plainHtmlArgsPrintMetaData = _htmlPrintMetadata,
              _plainHtmlArgsEntryPoint = m
            }


runCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runCommand HtmlOptions {..}
  | _htmlPlain = runSourceHtmlCommand HtmlOptions {..}
  | otherwise = _
    --  do
    --   ctx <- runPipeline _htmlInputFile upToInternalTyped
    --   outputDir <- someBaseToAbs' (_htmlOutputDir ^. pathPath)
    --   Html.genJudocHtml
    --     JudocArgs
    --       { _judocArgsOutputDir = outputDir,
    --         _judocArgsBaseName = "proj",
    --         _judocArgsCtx = ctx,
    --         _judocArgsAssetsDir = _htmlBaseUrl
    --       }
    --   when _htmlOpen $ case openCmd of
    --     Nothing -> say "Could not recognize the 'open' command for your OS"
    --     Just opencmd ->
    --       embed
    --         ( void
    --             ( Process.spawnProcess
    --                 opencmd
    --                 [ toFilePath
    --                     ( outputDir <//> Html.indexFileName
    --                     )
    --                 ]
    --             )
    --         )
