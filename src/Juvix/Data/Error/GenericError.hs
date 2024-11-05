module Juvix.Data.Error.GenericError
  ( module Juvix.Data.Error.GenericError,
  )
where

import Juvix.Data.Loc
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Prettyprinter.Render.Terminal qualified as Ansi
import System.Console.ANSI qualified as Ansi

data GenericError = GenericError
  { _genericErrorLoc :: Interval,
    _genericErrorMessage :: AnsiText,
    _genericErrorIntervals :: [Interval]
  }

newtype GenericOptions = GenericOptions
  { _showNameIds :: Bool
  }
  deriving stock (Eq, Show)

newtype SimpleError = SimpleError
  { _simpleErrorMessage :: AnsiText
  }

makeLenses ''GenericError
makeLenses ''GenericOptions
makeLenses ''SimpleError

defaultGenericOptions :: GenericOptions
defaultGenericOptions =
  GenericOptions
    { _showNameIds = False
    }

instance Pretty GenericError where
  pretty :: GenericError -> Doc a
  pretty g = genericErrorHeader g <> pretty (g ^. genericErrorMessage)

instance HasLoc GenericError where
  getLoc = (^. genericErrorLoc)

instance HasAnsiBackend SimpleError where
  toAnsiDoc (SimpleError msg) = toAnsiDoc msg
  toAnsiStream (SimpleError msg) = toAnsiStream msg

instance HasTextBackend SimpleError where
  toTextStream (SimpleError msg) = toTextStream msg
  toTextDoc (SimpleError msg) = toTextDoc msg

genericErrorHeader :: GenericError -> Doc a
genericErrorHeader g =
  pretty (g ^. genericErrorLoc)
    <> colon
    <+> "error"
      <> colon
      <> line

class ToGenericError a where
  genericError :: (Member (Reader GenericOptions) r) => a -> Sem r GenericError

instance ToGenericError GenericError where
  genericError = return

errorIntervals :: (ToGenericError e, Member (Reader GenericOptions) r) => e -> Sem r [Interval]
errorIntervals e = do
  e' <- genericError e
  return (e' ^. genericErrorIntervals)

render :: (ToGenericError e, Member (Reader GenericOptions) r) => Bool -> Maybe Char -> e -> Sem r Text
render ansi endChar err = do
  g <- genericError err
  let gMsg = g ^. genericErrorMessage
      header = genericErrorHeader g
      helper f x = (f . layoutPretty defaultLayoutOptions) (header <> x <> lastChar)
  if
      | ansi -> return $ helper Ansi.renderStrict (toAnsiDoc gMsg)
      | otherwise -> return $ helper renderStrict (toTextDoc gMsg)
  where
    lastChar :: Doc a
    lastChar = maybe "" pretty endChar

-- | Render the error to Text.
renderText :: (ToGenericError e, Member (Reader GenericOptions) r) => e -> Sem r Text
renderText = render False Nothing

renderTextDefault :: (ToGenericError e) => e -> Text
renderTextDefault = run . runReader defaultGenericOptions . renderText

-- | Render the error with Ansi formatting (if any).
renderAnsiText :: (ToGenericError e, Member (Reader GenericOptions) r) => e -> Sem r Text
renderAnsiText = render True Nothing

printErrorAnsi :: (ToGenericError e, Members '[EmbedIO, Reader GenericOptions] r) => e -> Sem r ()
printErrorAnsi e = renderAnsiText e >>= \txt -> hPutStrLn stderr txt

-- | Print the error to stderr without formatting.
printErrorText :: (ToGenericError e, Members '[EmbedIO, Reader GenericOptions] r) => e -> Sem r ()
printErrorText e = renderText e >>= \txt -> hPutStrLn stderr txt

printErrorAnsiSafe :: (ToGenericError e, Members '[EmbedIO, Reader GenericOptions] r) => e -> Sem r ()
printErrorAnsiSafe e =
  ifM
    (liftIO (Ansi.hSupportsANSIColor stderr))
    (printErrorAnsi e)
    (printErrorText e)

runErrorIO ::
  (ToGenericError a, Members '[EmbedIO, Reader GenericOptions] r) =>
  Sem (Error a ': r) b ->
  Sem r b
runErrorIO =
  runError >=> \case
    Left err -> printErrorAnsiSafe err >> exitFailure
    Right a -> return a

runErrorIO' ::
  (ToGenericError a, Member EmbedIO r) =>
  Sem (Error a ': r) b ->
  Sem r b
runErrorIO' = runReader defaultGenericOptions . runErrorIO . raiseUnder

runSimpleErrorIO :: (Members '[EmbedIO] r) => Sem (Error SimpleError ': r) a -> Sem r a
runSimpleErrorIO m = do
  res <- runError m
  case res of
    Left (SimpleError msg) -> hRenderIO True stderr msg >> exitFailure
    Right r -> return r
