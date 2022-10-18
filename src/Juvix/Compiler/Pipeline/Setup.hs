module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Prelude
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib

entrySetup ::
  Members '[Embed IO, Reader EntryPoint, Files] r =>
  Sem r EntryPoint
entrySetup = do
  e <- ask
  unless (e ^. entryPointNoStdlib) setupStdlib
  return e

setupStdlib ::
  Members '[Embed IO, Reader EntryPoint, Files] r =>
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
