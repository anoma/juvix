module BackendGeb.Compilation.Base where

import BackendGeb.FromCore.Base
import Base
import Juvix.Compiler.Backend (Target (TargetGeb))
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Pipeline

gebCompilationAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
gebCompilationAssertion mainFile expectedFile step = do
  step "Translate to JuvixCore"
  cwd <- getCurrentDir
  let entryPoint = (defaultEntryPoint cwd mainFile) {_entryPointTarget = TargetGeb}
  tab <- (^. Core.coreResultTable) . snd <$> runIO' iniState entryPoint upToCore
  coreToGebTranslationAssertion' tab entryPoint expectedFile step
