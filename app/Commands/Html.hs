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
import Juvix.Compiler.Internal.Translation
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context (resultInternal, resultNormalized)
import Juvix.Extra.Process
import System.Process qualified as Process

runGenOnlySourceHtml :: (Members '[Embed IO, TaggedLock, App] r) => HtmlOptions -> Sem r ()
runGenOnlySourceHtml HtmlOptions {..} = do
  res <- runPipeline _htmlInputFile upToScoping
  let m = res ^. Scoper.resultModule
  outputDir <- fromAppPathDir _htmlOutputDir
  embed $
    Html.genSourceHtml
      GenSourceHtmlArgs
        { _genSourceHtmlArgsAssetsDir = _htmlAssetsPrefix,
          _genSourceHtmlArgsHtmlKind = Html.HtmlOnly,
          _genSourceHtmlArgsOnlyCode = _htmlOnlyCode,
          _genSourceHtmlArgsParamBase = "",
          _genSourceHtmlArgsUrlPrefix = _htmlUrlPrefix,
          _genSourceHtmlArgsIdPrefix = _htmlIdPrefix,
          _genSourceHtmlArgsNoPath = _htmlNoPath,
          _genSourceHtmlArgsConcreteOpts = Concrete.defaultOptions,
          _genSourceHtmlArgsModule = m,
          _genSourceHtmlArgsComments = Scoper.getScoperResultComments res,
          _genSourceHtmlArgsOutputDir = outputDir,
          _genSourceHtmlArgsNoFooter = _htmlNoFooter,
          _genSourceHtmlArgsNonRecursive = _htmlNonRecursive,
          _genSourceHtmlArgsTheme = _htmlTheme
        }

resultToJudocCtx :: InternalTypedResult -> Html.JudocCtx
resultToJudocCtx res =
  Html.JudocCtx
    { _judocCtxComments = Scoper.getScoperResultComments sres,
      _judocCtxNormalizedTable = res ^. resultNormalized,
      _judocCtxTopModules = [sres ^. Scoper.resultModule]
    }
  where
    sres = res ^. resultInternal . resultScoper

runCommand :: forall r. (Members '[Embed IO, TaggedLock, App] r) => HtmlOptions -> Sem r ()
runCommand HtmlOptions {..}
  | _htmlOnlySource = runGenOnlySourceHtml HtmlOptions {..}
  | otherwise = do
      entry <- getEntryPoint _htmlInputFile
      (r, rs) <- runPipelineHtml _htmlNonRecursive _htmlInputFile
      outputDir <- fromAppPathDir _htmlOutputDir
      let ctx = resultToJudocCtx r <> mconcatMap resultToJudocCtx rs
      Html.genJudocHtml
        entry
        JudocArgs
          { _judocArgsAssetsPrefix = _htmlAssetsPrefix,
            _judocArgsBaseName = "proj",
            _judocArgsCtx = ctx,
            _judocArgsMainModule = r ^. resultInternal . resultScoper . Scoper.resultModule,
            _judocArgsOutputDir = outputDir,
            _judocArgsUrlPrefix = _htmlUrlPrefix,
            _judocArgsIdPrefix = _htmlIdPrefix,
            _judocArgsTheme = _htmlTheme,
            _judocArgsNonRecursive = _htmlNonRecursive,
            _judocArgsNoFooter = _htmlNoFooter,
            _judocArgsNoPath = _htmlNoPath
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
