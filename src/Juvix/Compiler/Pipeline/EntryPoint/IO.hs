module Juvix.Compiler.Pipeline.EntryPoint.IO where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

defaultEntryPointIO :: (Members '[Embed IO, TaggedLock, Final IO] r) => Path Abs Dir -> Path Abs File -> Sem r EntryPoint
defaultEntryPointIO cwd mainFile = do
  root <- findRootAndChangeDir (Just (parent mainFile)) Nothing cwd
  pkg <- readPackageRootIO root
  return (defaultEntryPoint pkg root mainFile)

defaultEntryPointNoFileIO :: (Members '[Embed IO, TaggedLock, Final IO] r) => Path Abs Dir -> Sem r EntryPoint
defaultEntryPointNoFileIO cwd = do
  root <- findRootAndChangeDir Nothing Nothing cwd
  pkg <- readPackageRootIO root
  return (defaultEntryPointNoFile pkg root)
