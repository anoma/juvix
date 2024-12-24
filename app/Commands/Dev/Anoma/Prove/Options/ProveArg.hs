module Commands.Dev.Anoma.Prove.Options.ProveArg
  ( ProveArg (..),
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

data ProveArg
  = ProveArgNat Natural
  | ProveArgBase64 (AppPath File)
  | ProveArgBytes (AppPath File)
  deriving stock (Data)

parseProveArg :: Parser ProveArg
parseProveArg = fromProveArg' <$> parseProveArg'
  where
    fromProveArg' :: ProveArg' -> ProveArg
    fromProveArg' (ProveArg' (ty :&: a)) = case ty of
      SProveArgTagNat -> ProveArgNat a
      SProveArgTagBase64 -> ProveArgBase64 a
      SProveArgTagBytes -> ProveArgBytes a

parseProveArg' :: Parser ProveArg'
parseProveArg' =
  option
    pp
    ( long "arg"
        <> completer (listCompleter [show a <> ":" | a <- allElements @ProveArgTag])
        <> metavar "ARG_TYPE:ARG"
        <> helpDoc ("An argument to the program." <> line <> enumHelp @ProveArgTag proveArgTagHelp)
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
        SProveArgTagBytes -> pAppPath
        SProveArgTagBase64 -> pAppPath
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
