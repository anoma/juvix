module Juvix.Compiler.Pipeline.EntryPoint.IO where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Root
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

defaultEntryPointIO' :: forall r. (Members '[EmbedIO, TaggedLock] r) => Path Abs Dir -> Maybe (Path Abs File) -> Sem r EntryPoint
defaultEntryPointIO' cwd mainFile = do
  root <- findRootAndChangeDir (parent <$> mainFile) Nothing cwd
  runReader (defaultEntryPoint packageBaseId root mainFile) (defaultEntryPointIO cwd mainFile)

defaultEntryPointIO :: forall r. (Members '[EmbedIO, TaggedLock, Reader EntryPoint] r) => Path Abs Dir -> Maybe (Path Abs File) -> Sem r EntryPoint
defaultEntryPointIO cwd mainFile = do
  root <- findRootAndChangeDir (parent <$> mainFile) Nothing cwd
  let pkgIdFromPackageFile :: Sem r PackageId
      pkgIdFromPackageFile = (^. packageId) <$> readPackageRootIO root
  pkgId <- case root ^. rootSomeRoot . someRootType of
    GlobalStdlib -> pkgIdFromPackageFile
    GlobalPackageDescription -> pkgIdFromPackageFile
    LocalPackage -> pkgIdFromPackageFile
    GlobalPackageBase -> return packageBaseId
  return (defaultEntryPoint pkgId root mainFile)
