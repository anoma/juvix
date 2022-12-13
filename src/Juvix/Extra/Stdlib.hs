module Juvix.Extra.Stdlib where

import Juvix.Data.Effect.Files
import Juvix.Extra.Paths
import Juvix.Extra.Version
import Juvix.Prelude

type RootPath = Path Abs Dir

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

writeStdlib :: forall r. Members '[Reader RootPath, Files] r => Sem r ()
writeStdlib = do
  rootDir <- ask
  forM_ (first (rootDir <//>) <$> stdlibFiles) (uncurry writeJuvixFile)
  where
    writeJuvixFile :: Path Abs File -> ByteString -> Sem r ()
    writeJuvixFile p bs = do
      ensureDir' (parent p)
      writeFileBS p bs

stdlibVersionFile :: Member (Reader RootPath) r => Sem r (Path Abs File)
stdlibVersionFile = (<//> parseRelFile' ".version") <$> ask

writeVersion :: forall r. Members '[Reader RootPath, Files] r => Sem r ()
writeVersion = stdlibVersionFile >>= flip writeFile' versionTag

readVersion :: Members '[Reader RootPath, Files] r => Sem r (Maybe Text)
readVersion = do
  vf <- stdlibVersionFile
  whenMaybeM (fileExists' vf) (readFile' vf)

updateStdlib :: forall r. Members '[Reader RootPath, Files] r => Sem r ()
updateStdlib =
  whenM shouldUpdate $ do
    whenM
      (ask @RootPath >>= directoryExists')
      (ask @RootPath >>= removeDirectoryRecursive')
    writeStdlib
    writeVersion
  where
    shouldUpdate :: Sem r Bool
    shouldUpdate =
      orM
        [ not <$> (ask @RootPath >>= directoryExists'),
          (Just versionTag /=) <$> readVersion
        ]
