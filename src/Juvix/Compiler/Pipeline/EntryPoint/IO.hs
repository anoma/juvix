module Juvix.Compiler.Pipeline.EntryPoint.IO where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Prelude

defaultEntryPointCwdIO :: Path Abs File -> IO EntryPoint
defaultEntryPointCwdIO mainFile = do
  cwd <- getCurrentDir
  roots <- findRootAndChangeDir (Just (parent mainFile)) Nothing cwd
  return (defaultEntryPoint roots mainFile)

defaultEntryPointNoFileCwdIO :: IO EntryPoint
defaultEntryPointNoFileCwdIO = do
  cwd <- getCurrentDir
  roots <- findRootAndChangeDir Nothing Nothing cwd
  return (defaultEntryPointNoFile roots)
