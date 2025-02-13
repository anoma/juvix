module Compilation.Base where

import Base
import Core.Compile.Base
import Core.Eval.Base
import Juvix.Compiler.Core qualified as Core

data CompileAssertionMode
  = EvalOnly
  | -- | Specify text to be sent to stdin of the process under test
    CompileOnly Text
  | EvalAndCompile

compileAssertion ::
  Path Abs Dir ->
  Int ->
  CompileAssertionMode ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion = compileAssertionEntry (set entryPointPipeline (Just PipelineExec))

compileAssertionEntry ::
  (EntryPoint -> EntryPoint) ->
  Path Abs Dir ->
  Int ->
  CompileAssertionMode ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertionEntry adjustEntry root' optLevel mode mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- adjustEntry <$> testDefaultEntryPointIO root' mainFile
  PipelineResult {..} <- snd <$> testRunIO entryPoint upToStoredCore
  let tab' = Core.computeCombinedInfoTable (_pipelineResult ^. Core.coreResultModule)
      evalAssertion = coreEvalAssertion' EvalModePlain tab' mainFile expectedFile step
      compileAssertion' stdinText = coreCompileAssertion' entryPoint optLevel tab' mainFile expectedFile stdinText step
  case mode of
    EvalOnly -> evalAssertion
    CompileOnly stdinText -> compileAssertion' stdinText
    EvalAndCompile -> evalAssertion >> compileAssertion' ""

compileErrorAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileErrorAssertion root' mainFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  res <- fmap snd <$> testRunIOEither entryPoint upToCore
  let noError :: Assertion = assertFailure "No error found"
  case res of
    Left {} -> return ()
    Right core -> do
      let res' =
            run
              . runError @JuvixError
              . runReader entryPoint
              $ Core.toStored (core ^. pipelineResult . Core.coreResultModule)
                >>= Core.toStripped Core.CheckExec
      case res' of
        Left {} -> return ()
        Right {} -> noError
