module Compilation.Base where

import Base
import Core.Compile.Base
import Core.Eval.Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Core.Pipeline qualified as Core
import Juvix.Compiler.Core.Translation.FromInternal.Data qualified as Core
import Juvix.Compiler.Pipeline
import Juvix.Data.PPOutput

compileAssertion ::
  Bool ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion onlyEval mainFile expectedFile step = do
  step "Translate to JuvixCore"
  cwd <- getCurrentDir
  let entryPoint = defaultEntryPoint cwd mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIO' iniState entryPoint upToCore
  case run $ runError $ Core.toEval tab of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right tab' -> do
      coreEvalAssertion' tab' mainFile expectedFile step
      unless onlyEval $
        coreCompileAssertion' tab' mainFile expectedFile step
