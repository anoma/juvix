module Commands.Dev.DevCompile.Rust where

import Commands.Base
import Commands.Dev.DevCompile.Rust.Options
import Commands.Extra.NewCompile
import Juvix.Compiler.Backend.Rust.Data.Result

runCommand ::
  (Members AppEffects r) =>
  RustOptions 'InputMain ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. rustCompileCommonOptions . compileInputFile
      moutputFile = opts ^. rustCompileCommonOptions . compileOutputFile
  outFile :: Path Abs File <- getOutputFile FileExtRust inputFile moutputFile
  Result {..} <- runPipeline opts inputFile upToRust
  writeFileEnsureLn outFile _resultRustCode
