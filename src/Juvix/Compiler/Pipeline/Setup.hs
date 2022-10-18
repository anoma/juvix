module Juvix.Compiler.Pipeline.Setup where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Extra.Paths
import Juvix.Extra.Stdlib
import Juvix.Prelude

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
      runReader d updateStdlib
      return d
    Just p -> return p
  registerStdlib stdlibRootPath
