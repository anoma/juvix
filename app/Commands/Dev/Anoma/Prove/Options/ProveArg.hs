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
import Juvix.Prelude.Parsing qualified as P

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
  | ProveArgList (AppPath File)
  | ProveArgFile ArgFileSpec
  deriving stock (Data)

parseProveArg :: Parser ProveArg
parseProveArg = fromProveArg' <$> parseProveArg'
  where
    fromProveArg' :: ProveArg' -> ProveArg
    fromProveArg' (ProveArg' (ty :&: a)) = case ty of
      SProveArgTagNat -> ProveArgNat a
      SProveArgTagList -> ProveArgList a
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

data NegativeError = NegativeError
  deriving stock (Show)

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
    pProveArgTag :: ParsecS r ProveArgTag
    pProveArgTag =
      choice
        [ chunk (show a) $> a
          | a :: ProveArgTag <- allElements
        ]

    pAppPath :: ParsecS r (AppPath File)
    pAppPath = do
      i <- mkPrepath . unpack <$> takeRest
      return
        AppPath
          { _pathIsInput = True,
            _pathPath = i
          }

    pProveArg' :: (Member (Error NegativeError) r) => ParsecS r ProveArg'
    pProveArg' = do
      dty <- pProveArgTag
      withSomeSing dty $ \ty -> do
        chunk ":"
        a <- pProveArgType ty
        return (ProveArg' (ty :&: a))

    pProveArgType :: (Member (Error NegativeError) r) => SProveArgTag t -> ParsecS r (ProveArgType t)
    pProveArgType p = do
      ret <- case p of
        SProveArgTagByteArray -> pAppPath
        SProveArgTagBytes -> pAppPath
        SProveArgTagList -> pAppPath
        SProveArgTagBase64 -> pAppPath
        SProveArgTagBase64UnJammed -> pAppPath
        SProveArgTagBytesUnJammed -> pAppPath
        SProveArgTagNat -> do
          i <- (^. withLocParam . integerWithBaseValue) <$> integerWithBase'
          if
              | i < 0 -> P.lift $ throw NegativeError
              | otherwise -> return (fromIntegral i)
      eof
      return ret

    pp :: ReadM ProveArg'
    pp = eitherReader $ \strInput ->
      let e =
            run
              . runError @NegativeError
              $ parseHelperS pProveArg' (pack strInput)
       in case e of
            Left _ -> Left "Expected a non-negative integer"
            Right (Left s) -> Left s
            Right (Right a) -> Right a
