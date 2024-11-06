module Commands.Dev.Nockma.Encode where

import Commands.Base
import Commands.Dev.Nockma.Encode.Options
import Data.ByteString qualified as B
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
    EncodeBytes -> do
      renderStdOutRaw (jamToByteString from)
    EncodeBase64 -> renderStdOut (encodeJam64 from)
    EncodeText -> renderStdOut (Nockma.ppSerialize from)
    EncodeDebug -> renderStdOut (Nockma.ppPrint from)
  where
    fromTextEncoding :: (Members '[App, EmbedIO] r') => Sem r' (Term Natural)
    fromTextEncoding = do
      bs <- getContents
      getRight (parseText bs)
