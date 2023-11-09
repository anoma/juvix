module Juvix.Compiler.Pipeline.EntryPoint.IO where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Prelude

defaultEntryPointIO :: Path Abs Dir -> Path Abs File -> IO EntryPoint
defaultEntryPointIO cwd mainFile = do
  root <- findRootAndChangeDir (Just (parent mainFile)) Nothing cwd
  return (defaultEntryPoint root mainFile)

defaultEntryPointNoFileIO :: Path Abs Dir -> IO EntryPoint
defaultEntryPointNoFileIO cwd = do
  root <- findRootAndChangeDir Nothing Nothing cwd
  return (defaultEntryPointNoFile root)
