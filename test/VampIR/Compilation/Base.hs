module VampIR.Compilation.Base where

import Base
import Core.VampIR.Base (coreVampIRAssertion')
import Juvix.Compiler.Core
import VampIR.Core.Base (VampirBackend (..), vampirAssertion')

vampirCompileAssertion :: Path Abs Dir -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vampirCompileAssertion root' mainFile dataFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  PipelineResult {..} <- snd <$> testRunIO entryPoint upToStoredCore
  let tab = computeCombinedInfoTable (_pipelineResult ^. coreResultModule)
  coreVampIRAssertion' tab toVampIRTransformations mainFile dataFile step
  vampirAssertion' entryPoint VampirHalo2 tab dataFile step

vampirCompileErrorAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
vampirCompileErrorAssertion root' mainFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  r <- testRunIOEither entryPoint upToStoredCore
  case r of
    Left _ -> return ()
    Right res ->
      let m = snd res ^. pipelineResult . coreResultModule
       in case run $ runReader entryPoint $ runError @JuvixError $ toVampIR m of
            Left _ -> return ()
            Right _ -> assertFailure "no error"
