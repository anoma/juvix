module Juvix.Extra.Files where

import Juvix.Data.Effect.Files
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Paths
import Juvix.Extra.Version
import Juvix.Prelude

type OutputRoot = Path Abs Dir

juvixFiles :: [(FilePath, ByteString)] -> [(Path Rel File, ByteString)]
juvixFiles fs = mapMaybe helper fs
  where
    helper :: (FilePath, ByteString) -> Maybe (Path Rel File, ByteString)
    helper (fp', bs)
      | isStdLibFile fp = Just (fp, bs)
      | otherwise = Nothing
      where
        fp :: Path Rel File
        fp = relFile fp'

    isStdLibFile :: Path Rel File -> Bool
    isStdLibFile = isJuvixFile .||. isYamlFile .||. isPackageFile

    isYamlFile :: Path Rel File -> Bool
    isYamlFile = (== juvixYamlFile)

    isPackageFile :: Path Rel File -> Bool
    isPackageFile = (== packageFilePath)

writeFiles :: forall r. (Members '[Reader OutputRoot, Files] r) => [(Path Rel File, ByteString)] -> Sem r ()
writeFiles fs = do
  rootDir <- ask
  forM_ (first (rootDir <//>) <$> fs) (uncurry writeJuvixFile)
  where
    writeJuvixFile :: Path Abs File -> ByteString -> Sem r ()
    writeJuvixFile p bs = do
      ensureDir' (parent p)
      writeFileBS p bs

versionFile :: (Member (Reader OutputRoot) r) => Sem r (Path Abs File)
versionFile = (<//> $(mkRelFile ".version")) <$> ask

writeVersion :: forall r. (Members '[Reader OutputRoot, Files] r) => Sem r ()
writeVersion = do
  vf <- versionFile
  ensureDir' (parent vf)
  writeFileEnsureLn' vf versionTag

readVersion :: (Members '[Reader OutputRoot, Files] r) => Sem r (Maybe Text)
readVersion = do
  vf <- versionFile
  whenMaybeM (fileExists' vf) (readFile' vf)

updateFiles :: (Members '[TaggedLock, Reader OutputRoot, Files] r) => (forall r0. (Members '[Files, Reader OutputRoot] r0) => Sem r0 ()) -> Sem r ()
updateFiles action = do
  root <- ask @OutputRoot
  withTaggedLockDir root . whenM shouldUpdate $ do
    whenM
      (directoryExists' root)
      (removeDirectoryRecursive' root)
    writeVersion
    action
  where
    shouldUpdate :: (Members '[Files, Reader OutputRoot] r) => Sem r Bool
    shouldUpdate =
      orM
        [ not <$> (ask @OutputRoot >>= directoryExists'),
          (Just versionTag /=) <$> readVersion
        ]
