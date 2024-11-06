module Commands.Dev.Nockma.Encode.Options where

import CommonOptions
import Prelude (show)

data EncodeType
  = EncodeBase64
  | EncodeBytes
  | EncodeDebug
  | EncodeText
  deriving stock (Eq, Enum, Bounded, Ord, Data)

instance Show EncodeType where
  show = \case
    EncodeBase64 -> "base64"
    EncodeBytes -> "bytes"
    EncodeDebug -> "debug"
    EncodeText -> "text"

instance Pretty EncodeType where
  pretty = pretty . Prelude.show

data NockmaEncodeOptions = NockmaEncodeOptions
  { _nockmaEncodeFrom :: EncodeType,
    _nockmaEncodeTo :: EncodeType
  }
  deriving stock (Data)

makeLenses ''NockmaEncodeOptions

base64Help :: AnsiDoc
base64Help = "Jam and Base 64 encoding"

bytesHelp :: AnsiDoc
bytesHelp = "Jam encoding"

encodingHelp :: Doc AnsiStyle
encodingHelp =
  enumHelp
    ( \case
        EncodeBase64 -> base64Help
        EncodeBytes -> bytesHelp
        EncodeDebug -> "Nockma code with annotations"
        EncodeText -> "Nockma code without annotations"
    )

parseNockmaEncodeOptions :: Parser NockmaEncodeOptions
parseNockmaEncodeOptions = do
  _nockmaEncodeFrom <-
    option
      (enumReader Proxy)
      ( long "to"
          <> metavar "ENCODING"
          <> completer (enumCompleter @EncodeType Proxy)
          <> helpDoc
            ( "Choose the source encoding.\n"
                <> encodingHelp
            )
      )

  _nockmaEncodeTo <-
    option
      (enumReader Proxy)
      ( long "from"
          <> metavar "ENCODING"
          <> completer (enumCompleter @EncodeType Proxy)
          <> helpDoc
            ( "Choose the target encoding.\n"
                <> encodingHelp
            )
      )
  pure NockmaEncodeOptions {..}
