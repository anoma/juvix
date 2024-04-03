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
  entryPoint <- set entryPointTarget (Just TargetGeb) <$> testDefaultEntryPointIO root mainFile
  m <- (^. pipelineResult . Core.coreResultModule) . snd <$> testRunIO entryPoint upToStoredCore
  coreToGebTranslationAssertion' (Core.computeCombinedInfoTable m) entryPoint expectedFile step
