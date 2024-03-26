module Casm.Compilation.Base where

import Base
import Casm.Run.Base
import Juvix.Compiler.Casm.Data.Result
import Juvix.Compiler.Casm.Pretty
import Juvix.Compiler.Core qualified as Core
import Juvix.Data.Field
import Juvix.Data.PPOutput

compileAssertion ::
  Path Abs Dir ->
  Int ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion = compileAssertionEntry (\e -> e {_entryPointFieldSize = cairoFieldSize})

compileAssertionEntry ::
  (EntryPoint -> EntryPoint) ->
  Path Abs Dir ->
  Int ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertionEntry adjustEntry root' optLevel mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- adjustEntry <$> testDefaultEntryPointIO root' mainFile
  PipelineResult {..} <- snd <$> testRunIO entryPoint upToStoredCore
  step "Translate to CASM"
  let entryPoint' = entryPoint {_entryPointOptimizationLevel = optLevel}
  case run $ runError @JuvixError $ runReader entryPoint' $ storedCoreToCasm (_pipelineResult ^. Core.coreResultModule) of
    Left err -> assertFailure (show (pretty (fromJuvixError @GenericError err)))
    Right Result {..} -> do
      withTempDir'
        ( \dirPath -> do
            let tmpFile = dirPath <//> $(mkRelFile "tmp.out")
            step "Serialize"
            writeFileEnsureLn tmpFile (toPlainText $ ppProgram _resultCode)
        )
      casmRunAssertion' False _resultLabelInfo _resultCode expectedFile step
