module Commands.Compile.Cairo where

import Commands.Base
import Commands.Compile.Cairo.Options
import Commands.Extra.NewCompile
import Data.Aeson qualified as JSON

runCommand :: (Members AppEffects r) => CairoOptions 'InputMain -> Sem r ()
runCommand opts = do
  let opts' = opts ^. cairoCompileCommonOptions
      inputFile = opts' ^. compileInputFile
      moutputFile = opts' ^. compileOutputFile
  r <- runError @JuvixError $ runPipeline opts (opts ^. cairoCompileCommonOptions . compileInputFile) upToCairo
  res <- getRight r
  cairoFile :: Path Abs File <- getOutputFile FileExtJson inputFile moutputFile
  liftIO (JSON.encodeFile (toFilePath cairoFile) res)
