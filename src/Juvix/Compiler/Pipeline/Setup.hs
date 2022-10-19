module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths
import Juvix.Prelude

entrySetup ::
  Members '[Reader EntryPoint, Files] r =>
  Sem r EntryPoint
entrySetup = do
  e <- ask
  unless (e ^. entryPointNoStdlib) setupStdlib
  return e

setupStdlib ::
  Members '[Reader EntryPoint, Files] r =>
  Sem r ()
setupStdlib = do
  e <- ask
  stdlibRootPath <- case e ^. entryPointStdlibPath of
    Nothing -> do
      let d = (e ^. entryPointRoot) </> juvixStdlibDir
      updateStdlib d
      return d
    Just p -> return p
  registerStdlib stdlibRootPath
