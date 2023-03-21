module Compilation.Base where

import Base
import Core.Compile.Base
import Core.Eval.Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Core.Options qualified as Core
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data qualified as Core
import Juvix.Compiler.Pipeline
import Juvix.Data.PPOutput

data CompileAssertionMode
  = EvalOnly
  | -- | Specify text to be sent to stdin of the process under test
    CompileOnly Text
  | EvalAndCompile

compileAssertion ::
  CompileAssertionMode ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion mode mainFile expectedFile step = do
  step "Translate to JuvixCore"
  cwd <- getCurrentDir
  let entryPoint = defaultEntryPoint cwd mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIO' iniState entryPoint upToCore
  case run $ runReader Core.defaultCoreOptions $ runError $ Core.toEval' tab of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right tab' -> do
      let evalAssertion = coreEvalAssertion' tab' mainFile expectedFile step
          compileAssertion' stdinText = coreCompileAssertion' tab' mainFile expectedFile stdinText step
      case mode of
        EvalOnly -> evalAssertion
        CompileOnly stdinText -> compileAssertion' stdinText
        EvalAndCompile -> evalAssertion >> compileAssertion' ""
