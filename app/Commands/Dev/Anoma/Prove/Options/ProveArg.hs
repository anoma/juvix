module Commands.Dev.Anoma.Prove.Options.ProveArg
  ( ProveArg (..),
    ArgFileSpec (..),
    EncodingLayout (..),
    DecodingLayout (..),
    parseProveArg,
  )
where

import Commands.Dev.Anoma.Prove.Options.ProveArgTag
import CommonOptions
import Juvix.Data.IntegerWithBase
import Juvix.Parser.Lexer
import Juvix.Prelude.Parsing hiding (many, option)

type Parse = Parsec Void Text

newtype ProveArg' = ProveArg'
  { _proveArg :: Sigma ProveArgTag ProveArgTypeSym0
  }

data EncodingLayout
  = EncodingBytes
  | EncodingBase64
  deriving stock (Data)

data DecodingLayout
  = -- | Argument should be decoded as an Atom
    DecodingAtom
  | -- | Argument is jammed and should not be decoded
    DecodingJammed
  | -- | Argument should be decoded as a ByteArray
    DecodingByteArray
  deriving stock (Data)

data ArgFileSpec = ArgFileSpec
  { _argFileSpecEncoding :: EncodingLayout,
    _argFileSpecDecoding :: DecodingLayout,
    _argFileSpecFile :: AppPath File
  }
  deriving stock (Data)

data ProveArg
  = ProveArgNat Natural
  | ProveArgFile ArgFileSpec
  deriving stock (Data)

parseProveArg :: Parser ProveArg
parseProveArg = fromProveArg' <$> parseProveArg'
  where
    fromProveArg' :: ProveArg' -> ProveArg
    fromProveArg' (ProveArg' (ty :&: a)) = case ty of
      SProveArgTagNat -> ProveArgNat a
      SProveArgTagByteArray -> fileHelper a EncodingBytes DecodingByteArray
      SProveArgTagBase64 -> fileHelper a EncodingBase64 DecodingJammed
      SProveArgTagBytes -> fileHelper a EncodingBytes DecodingJammed
      SProveArgTagBase64UnJammed -> fileHelper a EncodingBase64 DecodingAtom
      SProveArgTagBytesUnJammed -> fileHelper a EncodingBytes DecodingAtom
      where
        fileHelper :: AppPath File -> EncodingLayout -> DecodingLayout -> ProveArg
        fileHelper f l d =
          ProveArgFile
            ArgFileSpec
              { _argFileSpecEncoding = l,
                _argFileSpecFile = f,
                _argFileSpecDecoding = d
              }

parseProveArg' :: Parser ProveArg'
parseProveArg' =
  option
    pp
    ( long "arg"
        <> completer (listCompleter [show a <> ":" | a <- allElements @ProveArgTag])
        <> metavar "ARG_TYPE:ARG"
        <> helpDoc ("An argument to the program:" <> line <> proveArgTagHelp)
    )
  where
    pProveArgTag :: Parse ProveArgTag
    pProveArgTag =
      choice
        [ chunk (show a) $> a
          | a :: ProveArgTag <- allElements
        ]

    pAppPath :: Parse (AppPath File)
    pAppPath = do
      i <- mkPrepath . unpack <$> takeRest
      return
        AppPath
          { _pathIsInput = True,
            _pathPath = i
          }

    pProveArg' :: Parse ProveArg'
    pProveArg' = do
      dty <- pProveArgTag
      withSomeSing dty $ \ty -> do
        chunk ":"
        a <- pProveArgType ty
        return (ProveArg' (ty :&: a))

    pProveArgType :: SProveArgTag t -> Parse (ProveArgType t)
    pProveArgType p = do
      ret <- case p of
        SProveArgTagByteArray -> pAppPath
        SProveArgTagBytes -> pAppPath
        SProveArgTagBase64 -> pAppPath
        SProveArgTagBase64UnJammed -> pAppPath
        SProveArgTagBytesUnJammed -> pAppPath
        SProveArgTagNat -> do
          off <- getOffset
          i <- (^. withLocParam . integerWithBaseValue) <$> integerWithBase'
          if
              | i < 0 -> parseFailure off "Expected a non-negative integer"
              | otherwise -> return (fromIntegral i)
      eof
      return ret

    pp :: ReadM ProveArg'
    pp = eitherReader $ \strInput -> parseHelper pProveArg' (pack strInput)
