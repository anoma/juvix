module Commands.Html where

import Commands.Base
import Commands.Html.Options
import Juvix.Compiler.Backend.Html.Translation.FromTyped (JudocArgs (..))
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Html
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source
  ( GenSourceHtmlArgs (..),
  )
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Extra.Process
import System.Process qualified as Process

runGenSourceHtml :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runGenSourceHtml HtmlOptions {..} = do
  res <- runPipeline _htmlInputFile upToScoping
  let m = head (res ^. Scoper.resultModules)
  outputDir <- someBaseToAbs' (_htmlOutputDir ^. pathPath)
  embed $
    Html.genSourceHtml
      GenSourceHtmlArgs
        { _genSourceHtmlArgsAssetsDir = _htmlAssetsPrefix,
          _genSourceHtmlArgsHtmlKind = Html.HtmlSrc,
          _genSourceHtmlArgsParamBase = "",
          _genSourceHtmlArgsUrlPrefix = _htmlUrlPrefix,
          _genSourceHtmlArgsConcreteOpts = Concrete.defaultOptions,
          _genSourceHtmlArgsEntryPoint = m,
          _genSourceHtmlArgsOutputDir = outputDir,
          _genSourceHtmlArgsPrintMetaData = _htmlPrintMetadata,
          _genSourceHtmlArgsRecursive = _htmlRecursive,
          _genSourceHtmlArgsTheme = _htmlTheme
        }

runCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runCommand HtmlOptions {..}
  | _htmlPlain = runGenSourceHtml HtmlOptions {..}
  | otherwise =
      do
        ctx <- runPipeline _htmlInputFile upToInternalTyped
        outputDir <- someBaseToAbs' (_htmlOutputDir ^. pathPath)
        Html.genJudocHtml
          JudocArgs
            { _judocArgsAssetsDir = _htmlAssetsPrefix,
              _judocArgsBaseName = "proj",
              _judocArgsCtx = ctx,
              _judocArgsOutputDir = outputDir,
              _judocArgsUrlPrefix = _htmlUrlPrefix,
              _judocArgsTheme = _htmlTheme
            }
        when _htmlOpen $ case openCmd of
          Nothing -> say "Could not recognize the 'open' command for your OS"
          Just opencmd ->
            embed
              ( void
                  ( Process.spawnProcess
                      opencmd
                      [ toFilePath
                          ( outputDir <//> Html.indexFileName
                          )
                      ]
                  )
              )
