module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude

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
      let d = defaultStdlibPath (e ^. entryPointRoot)
      updateStdlib d
      getAbsPath d
    Just p -> getAbsPath p
  traceM ("stdlib at " <> pack stdlibRootPath)
  addDependency (Dependency stdlibRootPath)
