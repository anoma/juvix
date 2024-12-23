{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Commands.Dev.Anoma.Prove.Options where

import CommonOptions
import Juvix.Data.IntegerWithBase
import Juvix.Parser.Lexer
import Juvix.Prelude.Parsing hiding (option, many)
import Prelude qualified

data ProveArgTag
  = ProveArgNat
  | ProveArgBase64
  | ProveArgBytes
  deriving stock (Eq, Bounded, Enum, Data)

instance Show ProveArgTag where
  show = \case
    ProveArgNat -> "nat"
    ProveArgBase64 -> "base64"
    ProveArgBytes -> "bytes"

type ProveArgType :: ProveArgTag -> GHCType
type family ProveArgType s = res where
  ProveArgType 'ProveArgNat = Natural
  ProveArgType 'ProveArgBase64 = AppPath File
  ProveArgType 'ProveArgBytes = AppPath File

$(genDefunSymbols [''ProveArgType])

data ProveOptions = ProveOptions
  { _proveFile :: AppPath File,
    _proveArgs :: Maybe (AppPath File),
    _proveArgs2 :: [ProveArg],
    _proveOutputFile :: Maybe (AppPath File)
  }
  deriving stock (Data)

newtype ProveArg = ProveArg
  { _proveArg :: Sigma ProveArgTag ProveArgTypeSym0
  }

$(genSingletons [''ProveArgTag])

makeLenses ''ProveOptions
makeLenses ''ProveArg

type Parse = Parsec Void Text

parseProveArg :: Parser ProveArg
parseProveArg =
  option
    pp
    ( long "arg"
        <> completer (enumCompleter (Proxy @ProveArgTag))
        <> metavar "ARG_TYPE:ARG"
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

    pProveArg :: Parse ProveArg
    pProveArg = do
      dty <- pProveArgTag
      withSomeSing dty $ \ty -> do
        chunk ":"
        a <- pProveArgType ty
        return (ProveArg (ty :&: a))

    pProveArgType :: SProveArgTag t -> Parse (ProveArgType t)
    pProveArgType p = do
      ret <- case p of
        SProveArgBytes -> pAppPath
        SProveArgBase64 -> pAppPath
        SProveArgNat -> do
          off <- getOffset
          i <- (^. withLocParam . integerWithBaseValue) <$> integerWithBase'
          if
              | i < 0 -> parseFailure off "Expected a non-negative integer"
              | otherwise -> return (fromIntegral i)
      eof
      return ret

    pp :: ReadM ProveArg
    pp = eitherReader $ \strInput -> parseHelper pProveArg (pack strInput)

parseProveOptions :: Parser ProveOptions
parseProveOptions = do
  _proveFile <- parseInputFile FileExtNockma
  _proveArgs <- optional anomaArgsOpt
  _proveArgs2 <- many parseProveArg
  _proveOutputFile <- optional parseGenericOutputFile
  pure ProveOptions {..}
