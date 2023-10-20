module Compilation.Base where

import Base
import Core.Compile.Base
import Core.Eval.Base
import Juvix.Compiler.Core qualified as Core
import Juvix.Data.PPOutput

data CompileAssertionMode
  = EvalOnly
  | -- | Specify text to be sent to stdin of the process under test
    CompileOnly Text
  | EvalAndCompile

compileAssertion ::
  Int ->
  CompileAssertionMode ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion optLevel mode mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- defaultEntryPointCwdIO mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIO' entryPoint upToCore
  case run $ runReader Core.defaultCoreOptions $ runError $ Core.toEval' tab of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right tab' -> do
      let evalAssertion = coreEvalAssertion' EvalModePlain tab' mainFile expectedFile step
          compileAssertion' stdinText = coreCompileAssertion' optLevel tab' mainFile expectedFile stdinText step
      case mode of
        EvalOnly -> evalAssertion
        CompileOnly stdinText -> compileAssertion' stdinText
        EvalAndCompile -> evalAssertion >> compileAssertion' ""

compileErrorAssertion ::
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileErrorAssertion mainFile step = do
  step "Translate to JuvixCore"
  entryPoint <- defaultEntryPointCwdIO mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIO' entryPoint upToCore
  case run $ runReader Core.defaultCoreOptions $ runError @JuvixError $ Core.toStripped' tab of
    Left _ -> assertBool "" True
    Right _ -> assertFailure "no error"
