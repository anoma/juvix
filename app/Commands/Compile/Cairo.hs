module Commands.Compile.Cairo where

import Commands.Base
import Commands.Compile.Cairo.Options
import Commands.Extra.NewCompile
import Data.Aeson qualified as JSON

runCommand :: (Members '[App, TaggedLock, EmbedIO] r) => CairoOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. cairoCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  coreRes <- fromCompileCommonOptionsMain opts' >>= compileToCore
  entryPoint <-
    applyOptions opts
      <$> getEntryPoint (opts' ^. compileInputFile)
  cairoFile :: Path Abs File <- getOutputFile FileExtJson inputFile moutputFile
  r <-
    runReader entryPoint
      . runError @JuvixError
      . coreToCairo
      $ coreRes ^. coreResultModule
  res <- getRight r
  liftIO (JSON.encodeFile (toFilePath cairoFile) res)
