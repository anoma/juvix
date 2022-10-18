module Juvix.Extra.Stdlib where

import Data.ByteString qualified as BS
import Juvix.Extra.Paths
import Juvix.Prelude

stdlibFiles :: [(FilePath, ByteString)]
stdlibFiles = filter isJuvixFile $(stdlibDir)
  where
    isJuvixFile :: (FilePath, ByteString) -> Bool
    isJuvixFile (fp, _) = takeExtension fp == ".juvix"

writeStdlib :: forall r. Member (Embed IO) r => FilePath -> Sem r ()
writeStdlib rootDir = forM_ (first (rootDir </>) <$> stdlibFiles) (uncurry writeJuvixFile)
  where
    writeJuvixFile :: FilePath -> ByteString -> Sem r ()
    writeJuvixFile p bs = embed (createDirectoryIfMissing True (takeDirectory p) >> BS.writeFile p bs)

updateStdlib :: forall r. Member (Embed IO) r => FilePath -> Sem r ()
updateStdlib rootDir = unlessM (embed (doesDirectoryExist rootDir)) (writeStdlib rootDir)
