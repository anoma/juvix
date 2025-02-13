module Commands.Compile.Cairo where

import Commands.Base
import Commands.Compile.Cairo.Options
import Commands.Extra.NewCompile
import Data.Aeson qualified as JSON

runCommand :: (Members AppEffects r) => CairoOptions 'InputMain -> Sem r ()
runCommand CairoOptions {..} = do
  let inputFile = _cairoCompileCommonOptions ^. compileInputFile
      moutputFile = _cairoCompileCommonOptions ^. compileOutputFile
  copts <- fromCompileCommonOptionsMain _cairoCompileCommonOptions
  let opts' = CairoOptions {_cairoCompileCommonOptions = copts, ..}
  r <- runError @JuvixError $ runPipeline opts' inputFile upToCairo
  res <- getRight r
  cairoFile :: Path Abs File <- getOutputFile FileExtJson inputFile moutputFile
  liftIO (JSON.encodeFile (toFilePath cairoFile) res)
