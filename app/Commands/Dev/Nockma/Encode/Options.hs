module Commands.Dev.Nockma.Encode.Options where

import CommonOptions
import Prelude (show)

data EncodeType
  = EncodeBase64
  | EncodeBytes
  deriving stock (Eq, Enum, Bounded, Ord, Data)

defaultEncodeType :: EncodeType
defaultEncodeType = EncodeBase64

instance Show EncodeType where
  show = \case
    EncodeBase64 -> "base64"
    EncodeBytes -> "bytes"

data NockmaEncodeOptions = NockmaEncodeOptions
  { _nockmaEncodeFrom :: EncodeType,
    _nockmaEncodeTo :: EncodeType
  }
  deriving stock (Data)

makeLenses ''NockmaEncodeOptions

parseNockmaEncodeOptions :: Parser NockmaEncodeOptions
parseNockmaEncodeOptions = do
  _nockmaEncodeFrom <-
    option
      (enumReader Proxy)
      ( long "to"
          <> metavar "ENCODING"
          <> completer (enumCompleter @EncodeType Proxy)
          <> value defaultEncodeType
          <> helpDoc
            ( "Choose the source encoding.\n"
                <> enumHelp
                  ( \case
                      EncodeBase64 -> "Jam and Base 64 encoding"
                      EncodeBytes -> "Jam encoding"
                  )
            )
      )

  _nockmaEncodeTo <-
    option
      (enumReader Proxy)
      ( long "from"
          <> metavar "ENCODING"
          <> completer (enumCompleter @EncodeType Proxy)
          <> value defaultEncodeType
          <> helpDoc
            ( "Choose the target encoding.\n"
                <> enumHelp
                  ( \case
                      EncodeBase64 -> "Jam and Base 64 encoding"
                      EncodeBytes -> "Jam encoding"
                  )
            )
      )

  pure NockmaEncodeOptions {..}
