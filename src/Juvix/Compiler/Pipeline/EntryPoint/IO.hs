module Juvix.Compiler.Pipeline.EntryPoint.IO where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

defaultEntryPointIO :: Path Abs Dir -> Path Abs File -> IO EntryPoint
defaultEntryPointIO = defaultEntryPointIO' LockModePermissive

defaultEntryPointIO' :: LockMode -> Path Abs Dir -> Path Abs File -> IO EntryPoint
defaultEntryPointIO' lockMode cwd mainFile = do
  root <- findRootAndChangeDir lockMode (Just (parent mainFile)) Nothing cwd
  return (defaultEntryPoint root mainFile)

defaultEntryPointNoFileIO :: Path Abs Dir -> IO EntryPoint
defaultEntryPointNoFileIO = defaultEntryPointNoFileIO' LockModePermissive

defaultEntryPointNoFileIO' :: LockMode -> Path Abs Dir -> IO EntryPoint
defaultEntryPointNoFileIO' lockMode cwd = do
  root <- findRootAndChangeDir lockMode Nothing Nothing cwd
  return (defaultEntryPointNoFile root)
