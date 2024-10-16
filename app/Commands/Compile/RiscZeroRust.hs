module Commands.Compile.RiscZeroRust where

import Commands.Base
import Commands.Compile.RiscZeroRust.Options
import Commands.Compile.RiscZeroRust.Rust
import Juvix.Compiler.Backend.Rust.Data.Result

runCommand :: forall r. (Members AppEffects r) => RiscZeroRustOptions 'InputMain -> Sem r ()
runCommand opts = do
  let inputFile = opts ^. riscZeroRustCompileCommonOptions . compileInputFile
  Result {..} <- runPipeline opts inputFile upToRiscZeroRust
  compileRustCode opts inputFile _resultRustCode
