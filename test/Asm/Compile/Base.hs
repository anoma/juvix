module Asm.Compile.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Translation.FromSource
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Pipeline qualified as Pipeline
import Runtime.Base qualified as Runtime

asmCompileAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
asmCompileAssertion mainFile expectedFile step = do
  step "Parse"
  s <- readFile (toFilePath mainFile)
  case runParser (toFilePath mainFile) s of
    Left err -> assertFailure (show err)
    Right tab -> do
      step "Generate C code"
      case run $ runError @JuvixError $ Pipeline.asmToMiniC asmOpts tab of
        Left {} -> assertFailure "code generation failed"
        Right C.MiniCResult {..} ->
          withTempDir'
            ( \dirPath -> do
                let cFile = dirPath <//> replaceExtension' ".c" (filename mainFile)
                TIO.writeFile (toFilePath cFile) _resultCCode
                Runtime.clangAssertion cFile expectedFile "" step
            )
  where
    asmOpts :: Options
    asmOpts = makeOptions Backend.TargetCNative64 True
