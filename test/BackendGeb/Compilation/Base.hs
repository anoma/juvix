module BackendGeb.Compilation.Base where

import BackendGeb.FromCore.Base
import Base
import Juvix.Compiler.Backend (Target (TargetGeb))
import Juvix.Compiler.Core qualified as Core

gebCompilationAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
gebCompilationAssertion mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- set entryPointTarget TargetGeb <$> defaultEntryPointCwdIO mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIO' entryPoint upToCore
  coreToGebTranslationAssertion' tab entryPoint expectedFile step
