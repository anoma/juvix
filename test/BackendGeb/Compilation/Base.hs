module BackendGeb.Compilation.Base where

import BackendGeb.FromCore.Base
import Base
import Juvix.Compiler.Backend (Target (TargetGeb))
import Juvix.Compiler.Core qualified as Core
import Juvix.Data.Effect.TaggedLock

gebCompilationAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
gebCompilationAssertion root mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- set entryPointTarget TargetGeb <$> defaultEntryPointIO' LockModeExclusive root mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIOExclusive entryPoint upToCore
  coreToGebTranslationAssertion' tab entryPoint expectedFile step
