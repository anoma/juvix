module Juvix.Extra.Stdlib where

import Data.ByteString qualified as BS
import Juvix.Extra.Paths
import Juvix.Extra.Version
import Juvix.Prelude.Base

type RootPath = FilePath

stdlibFiles :: [(FilePath, ByteString)]
stdlibFiles = filter (isStdLibFile . fst) $(stdlibDir)
  where
    isStdLibFile :: FilePath -> Bool
    isStdLibFile = isJuvixFile .||. isYamlFile
    isJuvixFile :: FilePath -> Bool
    isJuvixFile fp = takeExtension fp == ".juvix"
    isYamlFile :: FilePath -> Bool
    isYamlFile = (== juvixYamlFile)

writeStdlib :: forall r. Members '[Reader RootPath, Embed IO] r => Sem r ()
writeStdlib = do
  rootDir <- ask
  forM_ (first (rootDir </>) <$> stdlibFiles) (uncurry writeJuvixFile)
  where
    writeJuvixFile :: FilePath -> ByteString -> Sem r ()
    writeJuvixFile p bs = embed (createDirectoryIfMissing True (takeDirectory p) >> BS.writeFile p bs)

stdlibVersionFile :: Member (Reader RootPath) r => Sem r FilePath
stdlibVersionFile = (</> ".version") <$> ask

writeVersion :: forall r. Members '[Reader RootPath, Embed IO] r => Sem r ()
writeVersion = (embed . flip writeFile versionTag) =<< stdlibVersionFile

readVersion :: Members '[Reader RootPath, Embed IO] r => Sem r (Maybe Text)
readVersion = do
  vf <- stdlibVersionFile
  embed (whenMaybeM (doesFileExist vf) (readFile vf))

updateStdlib :: forall r. Members '[Reader RootPath, Embed IO] r => Sem r ()
updateStdlib =
  whenM shouldUpdate $ do
    whenM
      (embed . doesDirectoryExist =<< ask)
      (embed . removeDirectoryRecursive =<< ask)
    writeStdlib
    writeVersion
  where
    shouldUpdate :: Sem r Bool
    shouldUpdate =
      orM
        [ not <$> (embed . doesDirectoryExist =<< ask),
          (Just versionTag /=) <$> readVersion
        ]
