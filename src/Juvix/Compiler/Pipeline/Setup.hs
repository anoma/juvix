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
registerDependencies = do
  root <- asks (^. entryPointRoot)
  let -- we register the entry root as a dependency
      entryDep :: Dependency
      entryDep = Dependency root
  addDependency entryDep

setupStdlib ::
  Members '[Reader EntryPoint, Files] r =>
  Sem r ()
setupStdlib = do
  e <- ask
  stdlibRootPath <- case e ^. entryPointStdlibPath of
    Nothing -> do
      let d = defaultStdlibPath (e ^. entryPointRoot)
      updateStdlib d
      return d
    Just p -> return p
  registerStdlib stdlibRootPath
