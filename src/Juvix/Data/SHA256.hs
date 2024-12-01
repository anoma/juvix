module Juvix.Data.SHA256 where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Juvix.Prelude

hashToText :: ByteString -> Text
hashToText =
  decodeUtf8Lenient
    . Base16.encode

digestText :: Text -> Text
digestText =
  hashToText
    . SHA256.hash
    . encodeUtf8

-- | Create a HEX encoded, SHA256 digest of the contents of a file
digestFile :: (Member Files r) => Path Abs File -> Sem r Text
digestFile = fmap hashToText . digestFileBS

digestFileBS :: (Member Files r) => Path Abs File -> Sem r ByteString
digestFileBS = fmap SHA256.hash . readFileBS'

data SHA256Builder :: Effect where
  BuilderDigestFiles :: (Foldable l) => l (Path Abs File) -> SHA256Builder m ()

makeSem ''SHA256Builder

builderDigestFile :: (Members '[SHA256Builder] r) => Path Abs File -> Sem r ()
builderDigestFile p = builderDigestFiles [p]

runSHA256Builder :: (Members '[Files] r) => Sem (SHA256Builder ': r) a -> Sem r (Text, a)
runSHA256Builder m = fmap
  ( first
      ( hashToText
          . SHA256.finalize
      )
  )
  $ reinterpret' m (runState SHA256.init)
  $ \case
    BuilderDigestFiles f -> do
      fs <- mapM readFileBS' (toList f)
      modify (\ctx -> SHA256.updates ctx (toList fs))

ignoreSHA256Builder :: Sem (SHA256Builder ': r) a -> Sem r a
ignoreSHA256Builder = interpret $ \case
  BuilderDigestFiles {} -> return ()

execSHA256Builder :: (Members '[Files] r) => Sem (SHA256Builder ': r) a -> Sem r Text
execSHA256Builder = fmap fst . runSHA256Builder

-- | Create a HEX encoded, SHA256 digest of the contents of some files
digestFiles :: (Members '[Files] r, Foldable l) => l (Path Abs File) -> Sem r Text
digestFiles = execSHA256Builder . digestFiles
