module Commands.Compile.Cairo where

import Commands.Base
import Commands.Compile.Cairo.Options
import Commands.Extra.NewCompile
import Data.Aeson qualified as JSON
import Juvix.Compiler.Backend

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => CairoOptions -> Sem r ()
runCommand opts = do
  let opts' = opts ^. cairoCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    set entryPointTarget (Just TargetCairo)
      . applyCompileCommonOptions opts'
      <$> getEntryPoint (opts' ^. compileInputFile)
  cairoFile :: Path Abs File <- getOutputFile FileExtCasm inputFile moutputFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToCairo
      $ coreRes ^. coreResultModule
  res <- getRight r
  liftIO (JSON.encodeFile (toFilePath cairoFile) res)
