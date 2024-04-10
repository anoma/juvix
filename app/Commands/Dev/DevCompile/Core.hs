module Commands.Dev.DevCompile.Core where

import Commands.Base
import Commands.Dev.DevCompile.Core.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation qualified as Core

compileTransformations :: [Core.TransformationId]
compileTransformations = [Core.CombineInfoTables, Core.FilterUnreachable, Core.DisambiguateNames]

runCommand ::
  forall r.
  (Members '[App, TaggedLock, EmbedIO] r) =>
  CoreOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. coreCompileCommonOptions . compileInputFile
      moutputFile = opts ^. coreCompileCommonOptions . compileOutputFile
  gopts <- askGlobalOptions
  outFile :: Path Abs File <- getOutputFile FileExtJuvixCore inputFile moutputFile
  coremodule :: Core.Module <- (^. coreResultModule) <$> runPipeline inputFile upToStoredCore
  res :: Core.Module <-
    ( runError @JuvixError
        . runReader (project' @Core.CoreOptions gopts)
        . Core.applyTransformations compileTransformations
        $ coremodule
      )
      >>= getRight
  let txt = ppPrint (res ^. Core.moduleInfoTable)
  writeFileEnsureLn outFile txt
