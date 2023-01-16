module Commands.Html where

import Commands.Base
import Commands.Html.Options
import Juvix.Compiler.Backend.Html.Translation.FromTyped (JudocArgs (..))
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Html
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source (GetPlainHtmlArgs (..))
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Extra.Process
import System.Process qualified as Process

runSourceHtmlCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runSourceHtmlCommand HtmlOptions {..} = do
  res <- runPipeline _htmlInputFile upToScoping
  let m = head (res ^. Scoper.resultModules)
  outputDir <- someBaseToAbs' (_htmlOutputDir ^. pathPath)
  embed $
    Html.genPlainHtml
      GetPlainHtmlArgs
        { _genPlainHtmlArgsAssetsDir = _htmlAssetsPrefix,
          _genPlainHtmlArgsHtmlKind = Html.HtmlSrc,
          _genPlainHtmlArgsParamBase = "",
          _genPlainHtmlArgsUrlPrefix = _htmlUrlPrefix,
          _getPlainHtmlArgsConcreteOpts = Concrete.defaultOptions,
          _getPlainHtmlArgsEntryPoint = m,
          _getPlainHtmlArgsOutputDir = outputDir,
          _getPlainHtmlArgsPrintMetaData = _htmlPrintMetadata,
          _getPlainHtmlArgsRecursive = _htmlRecursive,
          _getPlainHtmlArgsTheme = _htmlTheme
        }

runCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runCommand HtmlOptions {..}
  | _htmlPlain = runSourceHtmlCommand HtmlOptions {..}
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
