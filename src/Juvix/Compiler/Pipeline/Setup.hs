module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib
import Juvix.Prelude

entrySetup ::
  Members '[Reader EntryPoint, Files, PathResolver] r =>
  Sem r EntryPoint
entrySetup = do
  e <- ask
  registerDependencies
  return e

registerDependencies :: Members '[Reader EntryPoint, PathResolver] r => Sem r ()
registerDependencies = do
  e <- ask
  addDependency (Just e) (Dependency (e ^. entryPointRoot))

stdlibDep ::
  forall r.
  Members '[Reader EntryPoint, Files, PathResolver] r =>
  Sem r Dependency
stdlibDep = Dependency <$> getRoot
  where
    getRoot :: Sem r (Path Abs Dir)
    getRoot = do
      e <- ask
      case e ^. entryPointStdlibPath of
        Nothing -> do
          let d :: Path Abs Dir
              d = defaultStdlibPath (e ^. entryPointRoot)
          runReader d updateStdlib
          return d
        Just p -> return p
