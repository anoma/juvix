module VampIR.Compilation.Base where

import Base
import Core.VampIR.Base (coreVampIRAssertion')
import Juvix.Compiler.Core
import Juvix.Compiler.Core.Data.TransformationId
import VampIR.Core.Base (VampirBackend (..), vampirAssertion')

vampirCompileAssertion :: Path Abs Dir -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vampirCompileAssertion root' mainFile dataFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  tab <- (^. coreResultTable) . snd <$> testRunIO entryPoint upToCore
  coreVampIRAssertion' tab toVampIRTransformations mainFile dataFile step
  vampirAssertion' VampirHalo2 tab dataFile step

vampirCompileErrorAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
vampirCompileErrorAssertion root' mainFile step = do
  step "Translate to JuvixCore"
  entryPoint <- testDefaultEntryPointIO root' mainFile
  r <- snd <$> testRunIOEither entryPoint upToCore
  case r of
    Left _ -> return ()
    Right res ->
      let tab = snd res ^. coreResultTable
       in case run $ runReader defaultCoreOptions $ runError @JuvixError $ toVampIR' tab of
            Left _ -> return ()
            Right _ -> assertFailure "no error"
