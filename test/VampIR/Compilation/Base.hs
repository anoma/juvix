module VampIR.Compilation.Base where

import Base
import Core.VampIR.Base (coreVampIRAssertion')
import Juvix.Compiler.Core
import Juvix.Compiler.Core.Data.TransformationId
import VampIR.Core.Base (vampirAssertion')

vampirCompileAssertion :: Int -> Path Abs File -> Path Abs File -> (String -> IO ()) -> Assertion
vampirCompileAssertion paramsNum mainFile dataFile step = do
  step "Translate to JuvixCore"
  entryPoint <- defaultEntryPointCwdIO mainFile
  tab <- (^. coreResultTable) . snd <$> runIO' entryPoint (upToCore FilterUnreachable)
  coreVampIRAssertion' tab toVampIRTransformations mainFile dataFile step
  vampirAssertion' paramsNum tab dataFile step
