module Asm.Compile.Base where

import Base
import Data.Text.IO qualified as TIO
import Juvix.Compiler.Asm.Data.InfoTable
import Juvix.Compiler.Asm.Error
import Juvix.Compiler.Asm.Options
import Juvix.Compiler.Asm.Translation.FromSource
import Juvix.Compiler.Backend qualified as Backend
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Pipeline qualified as Pipeline
import Runtime.Base qualified as Runtime

asmCompileAssertion' :: InfoTable -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
asmCompileAssertion' tab mainFile expectedFile step = do
  step "Generate C code"
  case run $ runError @JuvixError $ Pipeline.asmToMiniC asmOpts tab of
    Left e -> do
      let err :: AsmError = fromJust (fromJuvixError e)
      assertFailure ("code generation failed:" <> "\n" <> unpack (err ^. asmErrorMsg))
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

asmCompileAssertion :: Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
asmCompileAssertion mainFile expectedFile step = do
  step "Parse"
  s <- readFile (toFilePath mainFile)
  case runParser (toFilePath mainFile) s of
    Left err -> assertFailure (show err)
    Right tab -> asmCompileAssertion' tab mainFile expectedFile step
