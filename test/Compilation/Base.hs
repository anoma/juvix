module Compilation.Base where

import Data.HashMap.Strict qualified as HashMap
import Base
import Juvix.Compiler.Builtins (iniState)
import Juvix.Compiler.Pipeline
import Juvix.Compiler.Core.Translation.FromInternal.Data qualified as Core
import Juvix.Compiler.Core.Data.InfoTable qualified as Core

compileAssertion ::
  Path Abs File ->
  Path Abs File ->
  (String -> IO ()) ->
  Assertion
compileAssertion mainFile _ step = do
  step "Translate to JuvixCore"
  cwd <- getCurrentDir
  let entryPoint = defaultEntryPoint cwd mainFile
  tab <- (^. Core.coreResultTable) . snd <$> runIO' iniState entryPoint upToCore
  case (tab ^. Core.infoMain) >>= ((tab ^. Core.identContext) HashMap.!?) of
    Just _ -> return ()
    Nothing -> assertFailure ("No main function registered in: " <> toFilePath mainFile)
