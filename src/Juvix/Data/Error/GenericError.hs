module Juvix.Data.Error.GenericError
  ( module Juvix.Data.Error.GenericError,
  )
where

import Juvix.Data.Loc
import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text
import System.Console.ANSI qualified as Ansi

data GenericError = GenericError
  { _genericErrorLoc :: Interval,
    _genericErrorMessage :: AnsiText,
    _genericErrorIntervals :: [Interval]
  }

data GenericOptions = GenericOptions
  { _showNameIds :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''GenericError
makeLenses ''GenericOptions

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

genericErrorHeader :: GenericError -> Doc a
genericErrorHeader g =
  pretty (g ^. genericErrorLoc)
    <> colon
    <+> "error"
      <> colon
      <> line

class ToGenericError a where
  genericError :: (Member (Reader GenericOptions) r) => a -> Sem r GenericError

errorIntervals :: (ToGenericError e, Member (Reader GenericOptions) r) => e -> Sem r [Interval]
errorIntervals e = do
  e' <- genericError e
  return (e' ^. genericErrorIntervals)

render :: (ToGenericError e, Member (Reader GenericOptions) r) => Bool -> Bool -> e -> Sem r Text
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
    lastChar
      | endChar = "ת"
      | otherwise = ""

-- | Render the error to Text.
renderText :: (ToGenericError e, Member (Reader GenericOptions) r) => e -> Sem r Text
renderText = render False False

-- | Render the error with Ansi formatting (if any).
renderAnsiText :: (ToGenericError e, Member (Reader GenericOptions) r) => e -> Sem r Text
renderAnsiText = render True False

printErrorAnsi :: (ToGenericError e, Members '[Embed IO, Reader GenericOptions] r) => e -> Sem r ()
printErrorAnsi e = renderAnsiText e >>= \txt -> embed (hPutStrLn stderr txt)

-- | Print the error to stderr without formatting.
printErrorText :: (ToGenericError e, Members '[Embed IO, Reader GenericOptions] r) => e -> Sem r ()
printErrorText e = renderText e >>= \txt -> embed (hPutStrLn stderr txt)

printErrorAnsiSafe :: (ToGenericError e, Members '[Embed IO, Reader GenericOptions] r) => e -> Sem r ()
printErrorAnsiSafe e =
  ifM
    (embed (Ansi.hSupportsANSIColor stderr))
    (printErrorAnsi e)
    (printErrorText e)

runErrorIO ::
  (ToGenericError a, Members '[Embed IO, Reader GenericOptions] r) =>
  Sem (Error a ': r) b ->
  Sem r b
runErrorIO =
  runError >=> \case
    Left err -> printErrorAnsiSafe err >> embed exitFailure
    Right a -> return a

runErrorIO' ::
  (ToGenericError a, Member (Embed IO) r) =>
  Sem (Error a : r) b ->
  Sem r b
runErrorIO' = runReader defaultGenericOptions . runErrorIO . raiseUnder
