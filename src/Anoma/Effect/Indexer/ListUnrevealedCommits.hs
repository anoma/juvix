module Anoma.Effect.Indexer.ListUnrevealedCommits where

import Anoma.Effect.Base
import Anoma.Http.Indexer.ListUnrevealedCommits
import Data.ByteString.Base64 qualified as Base64
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language qualified as Nockma
import Juvix.Compiler.Nockma.Pretty
import Juvix.Prelude
import Juvix.Prelude.Aeson (Value)

newtype ListUnrevealedCommitsResult = ListUnrevealedCommitsResult
  {_listUnrevealedCommitsResultCommits :: [Nockma.Term Natural]}

makeLenses ''ListUnrevealedCommitsResult

listUnrevealedCommits ::
  forall r.
  (Members '[Anoma, Error SimpleError, Logger] r) =>
  Sem r ListUnrevealedCommitsResult
listUnrevealedCommits = do
  resVal :: Value <- anomaGet listUnrevealedCommitsUrl >>= fromJSONErr
  logMessageValue "Response Payload" resVal
  res :: Response <- fromJSONErr resVal
  commitBs :: [ByteString] <- mapM decodeCommit (res ^. responseCommits)
  commits :: [Atom Natural] <-
    mapError @NockNaturalNaturalError
      (SimpleError . mkAnsiText @Text . show)
      (mapM byteStringToAtom commitBs)
  return ListUnrevealedCommitsResult {_listUnrevealedCommitsResultCommits = TermAtom <$> commits}
  where
    decodeCommit :: Text -> Sem r ByteString
    decodeCommit t = case (Base64.decode (encodeUtf8 t)) of
      Left e -> throw (SimpleError (mkAnsiText ("Failed to decode commitment: " <> pack e)))
      Right bs -> return bs
