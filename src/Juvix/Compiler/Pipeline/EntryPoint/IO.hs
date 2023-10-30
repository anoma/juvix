module Juvix.Compiler.Pipeline.EntryPoint.IO where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Prelude

defaultEntryPointCwdIO :: Path Abs File -> IO EntryPoint
defaultEntryPointCwdIO mainFile = do
  cwd <- getCurrentDir
  root <- findRootAndChangeDir (Just (parent mainFile)) Nothing cwd
  return (defaultEntryPoint root mainFile)

defaultEntryPointNoFileCwdIO :: IO EntryPoint
defaultEntryPointNoFileCwdIO = do
  cwd <- getCurrentDir
  root <- findRootAndChangeDir Nothing Nothing cwd
  return (defaultEntryPointNoFile root)
