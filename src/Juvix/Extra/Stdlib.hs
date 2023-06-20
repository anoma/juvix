module Juvix.Extra.Stdlib where

import Juvix.Compiler.Pipeline.Package.Dependency
import Juvix.Data.Effect.Files
import Juvix.Extra.Paths
import Juvix.Extra.Version
import Juvix.Prelude

type StdlibRoot = Path Abs Dir

stdlibFiles :: [(Path Rel File, ByteString)]
stdlibFiles = mapMaybe helper $(stdlibDir)
  where
    helper :: (FilePath, ByteString) -> Maybe (Path Rel File, ByteString)
    helper (fp', bs)
      | isStdLibFile fp = Just (fp, bs)
      | otherwise = Nothing
      where
        fp :: Path Rel File
        fp = relFile fp'
    isStdLibFile :: Path Rel File -> Bool
    isStdLibFile = isJuvixFile .||. isYamlFile
    isYamlFile :: Path Rel File -> Bool
    isYamlFile = (== juvixYamlFile)

ensureStdlib :: Members '[Files] r => Path Abs Dir -> Path Abs Dir -> [Dependency] -> Sem r ()
ensureStdlib rootDir buildDir deps =
  whenJustM (packageStdlib rootDir buildDir deps) $ \stdlibRoot ->
    runReader stdlibRoot updateStdlib

packageStdlib :: forall r. Members '[Files] r => Path Abs Dir -> Path Abs Dir -> [Dependency] -> Sem r (Maybe (Path Abs Dir))
packageStdlib rootDir buildDir = firstJustM isStdLib
  where
    isStdLib :: Dependency -> Sem r (Maybe (Path Abs Dir))
    isStdLib = \case
      DependencyPath dep -> do
        adir <- canonicalDir rootDir (dep ^. pathDependencyPath)
        let mstdlib :: Maybe (Path Rel Dir) = stripProperPrefix buildDir adir
        return $
          if
              | mstdlib == Just relStdlibDir -> Just stdLibBuildDir
              | otherwise -> Nothing
        where
          stdLibBuildDir :: Path Abs Dir
          stdLibBuildDir = juvixStdlibDir buildDir
      DependencyGit {} -> return Nothing

writeStdlib :: forall r. (Members '[Reader StdlibRoot, Files] r) => Sem r ()
writeStdlib = do
  rootDir <- ask
  forM_ (first (rootDir <//>) <$> stdlibFiles) (uncurry writeJuvixFile)
  where
    writeJuvixFile :: Path Abs File -> ByteString -> Sem r ()
    writeJuvixFile p bs = do
      ensureDir' (parent p)
      writeFileBS p bs

stdlibVersionFile :: (Member (Reader StdlibRoot) r) => Sem r (Path Abs File)
stdlibVersionFile = (<//> $(mkRelFile ".version")) <$> ask

writeVersion :: forall r. (Members '[Reader StdlibRoot, Files] r) => Sem r ()
writeVersion = stdlibVersionFile >>= flip writeFile' versionTag

readVersion :: (Members '[Reader StdlibRoot, Files] r) => Sem r (Maybe Text)
readVersion = do
  vf <- stdlibVersionFile
  whenMaybeM (fileExists' vf) (readFile' vf)

updateStdlib :: forall r. (Members '[Reader StdlibRoot, Files] r) => Sem r ()
updateStdlib =
  whenM shouldUpdate $ do
    whenM
      (ask @StdlibRoot >>= directoryExists')
      (ask @StdlibRoot >>= removeDirectoryRecursive')
    writeStdlib
    writeVersion
  where
    shouldUpdate :: Sem r Bool
    shouldUpdate =
      orM
        [ not <$> (ask @StdlibRoot >>= directoryExists'),
          (Just versionTag /=) <$> readVersion
        ]
