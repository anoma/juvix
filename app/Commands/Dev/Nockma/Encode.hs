module Commands.Dev.Nockma.Encode where

import Commands.Base
import Commands.Dev.Nockma.Encode.Options
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as Base64
import Juvix.Compiler.Nockma.Encoding
import Juvix.Compiler.Nockma.Language (Term)
import Juvix.Compiler.Nockma.Pretty qualified as Nockma
import Juvix.Compiler.Nockma.Translation.FromSource.Base

runCommand :: forall r. (Members AppEffects r) => NockmaEncodeOptions -> Sem r ()
runCommand opts = runSimpleErrorIO $ do
  from :: Term Natural <- case opts ^. nockmaEncodeFrom of
    EncodeBytes -> do
      bs <- liftIO B.getContents
      decodeCue bs
    EncodeBase64 -> do
      bs <- getContents
      decodeCue64 bs
    EncodeText -> fromTextEncoding
    EncodeDebug -> fromTextEncoding
  case opts ^. nockmaEncodeTo of
    EncodeBytes ->
      case opts ^. nockmaEncodeApply of
        Nothing -> renderStdOutRaw (jamToByteString from)
        Just Cue -> termToByteString from >>= renderStdOutRaw
    EncodeBase64 ->
      case opts ^. nockmaEncodeApply of
        Nothing -> renderStdOut (encodeJam64 from)
        Just Cue ->
          termToByteString from
            >>= renderStdOut
              . decodeUtf8
              . Base64.encode
    EncodeText ->
      case opts ^. nockmaEncodeApply of
        Nothing -> renderStdOut (Nockma.ppSerialize from)
        Just Cue -> termToByteString from >>= decodeCue >>= renderStdOut . Nockma.ppSerialize
    EncodeDebug ->
      case opts ^. nockmaEncodeApply of
        Nothing -> renderStdOut (Nockma.ppPrint from)
        Just Cue -> termToByteString from >>= decodeCue >>= renderStdOut . Nockma.ppPrint
  where
    termToByteString :: (Members '[Error SimpleError] r') => Term Natural -> Sem r' ByteString
    termToByteString = \case
      Nockma.TermCell {} -> throw @SimpleError "Cannot cue a cell"
      Nockma.TermAtom a -> return (naturalToByteString (a ^. Nockma.atom))

    fromTextEncoding :: (Members '[App, EmbedIO] r') => Sem r' (Term Natural)
    fromTextEncoding = do
      bs <- getContents
      getRight (parseText bs)
