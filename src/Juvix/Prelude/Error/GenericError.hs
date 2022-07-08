module Juvix.Prelude.Error.GenericError
  ( module Juvix.Prelude.Error.GenericError,
  )
where

import Juvix.Prelude.Base
import Juvix.Prelude.Pretty
import Juvix.Syntax.Concrete.Loc
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text
import System.Console.ANSI qualified as Ansi

data GenericError = GenericError
  { _genericErrorLoc :: Interval,
    _genericErrorMessage :: AnsiText,
    _genericErrorIntervals :: [Interval]
  }

makeLenses ''GenericError

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
  genericError :: a -> GenericError

errorIntervals :: ToGenericError e => e -> [Interval]
errorIntervals = (^. genericErrorIntervals) . genericError

render :: ToGenericError e => Bool -> Bool -> e -> Text
render ansi endChar err
  | ansi = helper Ansi.renderStrict (toAnsiDoc gMsg)
  | otherwise = helper renderStrict (toTextDoc gMsg)
  where
    helper :: (SimpleDocStream a -> Text) -> Doc a -> Text
    helper f x = (f . layoutPretty defaultLayoutOptions) (header <> x <> lastChar)

    g :: GenericError
    g = genericError err

    gMsg :: AnsiText
    gMsg = g ^. genericErrorMessage

    header :: Doc a
    header = genericErrorHeader g

    lastChar :: Doc a
    lastChar
      | endChar = "ת"
      | otherwise = ""

-- | Render the error to Text.
renderText :: ToGenericError e => e -> Text
renderText = render False False

-- | Render the error with Ansi formatting (if any).
renderAnsiText :: ToGenericError e => e -> Text
renderAnsiText = render True False

printErrorAnsi :: ToGenericError e => e -> IO ()
printErrorAnsi = hPutStrLn stderr . renderAnsiText

-- | Print the error to stderr without formatting.
printErrorText :: ToGenericError e => e -> IO ()
printErrorText = hPutStrLn stderr . renderText

printErrorAnsiSafe :: ToGenericError e => e -> IO ()
printErrorAnsiSafe e =
  ifM
    (Ansi.hSupportsANSI stderr)
    (printErrorAnsi e)
    (printErrorText e)

runErrorIO ::
  (ToGenericError a, Member (Embed IO) r) =>
  Sem (Error a ': r) b ->
  Sem r b
runErrorIO =
  runError >=> \case
    Left err -> embed (printErrorAnsiSafe err >> exitFailure)
    Right a -> return a
