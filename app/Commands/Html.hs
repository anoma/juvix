module Commands.Html where

import Commands.Base
import Commands.Html.Options
import Juvix.Compiler.Backend.Html.Translation.FromTyped (JudocArgs (..))
import Juvix.Compiler.Backend.Html.Translation.FromTyped qualified as Html
import Juvix.Compiler.Backend.Html.Translation.FromTyped.Source
 (GenHtmlArgs(..))
import Juvix.Compiler.Concrete.Pretty qualified as Concrete
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Extra.Process
import System.Process qualified as Process

runGenHtmlCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runGenHtmlCommand HtmlOptions {..} = do
  res <- runPipeline _htmlInputFile upToScoping
  let m = head (res ^. Scoper.resultModules)
  outputDir <- someBaseToAbs' (_htmlOutputDir ^. pathPath)
  embed $
    Html.genHtml
      GenHtmlArgs
        { _genHtmlArgsAssetsDir = _htmlAssetsPrefix,
          _genHtmlArgsHtmlKind = Html.HtmlSrc,
          _genHtmlArgsParamBase = "",
          _genHtmlArgsUrlPrefix = _htmlUrlPrefix,
          _genHtmlArgsConcreteOpts = Concrete.defaultOptions,
          _genHtmlArgsEntryPoint = m,
          _genHtmlArgsOutputDir = outputDir,
          _genHtmlArgsPrintMetaData = _htmlPrintMetadata,
          _genHtmlArgsRecursive = _htmlRecursive,
          _genHtmlArgsTheme = _htmlTheme
        }

runCommand :: Members '[Embed IO, App] r => HtmlOptions -> Sem r ()
runCommand HtmlOptions {..}
  | _htmlPlain = runGenHtmlCommand HtmlOptions {..}
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
