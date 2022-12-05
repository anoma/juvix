module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Stdlib
import Juvix.Prelude
import Juvix.Prelude.Path

entrySetup ::
  Members '[Reader EntryPoint, Files, PathResolver] r =>
  Sem r EntryPoint
entrySetup = do
  e <- ask
  unless (e ^. entryPointNoStdlib) setupStdlib
  registerDependencies
  return e

registerDependencies :: Members '[Reader EntryPoint, PathResolver] r => Sem r ()
registerDependencies = asks (^. entryPointRoot) >>= addDependency . Dependency

setupStdlib ::
  Members '[Reader EntryPoint, Files, PathResolver] r =>
  Sem r ()
setupStdlib = do
  e <- ask
  stdlibRootPath <- case e ^. entryPointStdlibPath of
    Nothing -> do
      let
        d :: Path Abs Dir
        d = defaultStdlibPath (absDir (e ^. entryPointRoot))
      runReader d updateStdlib
      getAbsPath d
    Just p -> getAbsPath p
  traceM ("stdlib at " <> pack stdlibRootPath)
  addDependency (Dependency stdlibRootPath)
