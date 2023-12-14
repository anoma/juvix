module BackendGeb.Compilation.Base where

import BackendGeb.FromCore.Base
import Base
import Juvix.Compiler.Backend (Target (TargetGeb))
import Juvix.Compiler.Core qualified as Core

gebCompilationAssertion ::
  Path Abs Dir ->
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
gebCompilationAssertion root mainFile expectedFile step = do
  step "Translate to JuvixCore"
  entryPoint <- set entryPointTarget TargetGeb <$> testDefaultEntryPointIO root mainFile
  tab <- (^. Core.coreResultTable) . snd <$> testRunIO entryPoint upToCore
  coreToGebTranslationAssertion' tab entryPoint expectedFile step
