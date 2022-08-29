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

newtype GenericOptions = GenericOptions
  { _optShowNameIds :: Bool
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
  genericError :: GenericOptions -> a -> GenericError

errorIntervals :: ToGenericError e => GenericOptions -> e -> [Interval]
errorIntervals opts = (^. genericErrorIntervals) . genericError opts

render :: ToGenericError e => GenericOptions -> Bool -> Bool -> e -> Text
render opts ansi endChar err
  | ansi = helper Ansi.renderStrict (toAnsiDoc gMsg)
  | otherwise = helper renderStrict (toTextDoc gMsg)
  where
    helper :: (SimpleDocStream a -> Text) -> Doc a -> Text
    helper f x = (f . layoutPretty defaultLayoutOptions) (header <> x <> lastChar)

    g :: GenericError
    g = genericError opts err

    gMsg :: AnsiText
    gMsg = g ^. genericErrorMessage

    header :: Doc a
    header = genericErrorHeader g

    lastChar :: Doc a
    lastChar
      | endChar = "ת"
      | otherwise = ""

-- | Render the error to Text.
renderText :: ToGenericError e => GenericOptions -> e -> Text
renderText opts = render opts False False

-- | Render the error with Ansi formatting (if any).
renderAnsiText :: ToGenericError e => GenericOptions -> e -> Text
renderAnsiText opts = render opts True False

printErrorAnsi :: ToGenericError e => GenericOptions -> e -> IO ()
printErrorAnsi opts = hPutStrLn stderr . renderAnsiText opts

-- | Print the error to stderr without formatting.
printErrorText :: ToGenericError e => GenericOptions -> e -> IO ()
printErrorText opts = hPutStrLn stderr . renderText opts

printErrorAnsiSafe :: ToGenericError e => GenericOptions -> e -> IO ()
printErrorAnsiSafe opts e =
  ifM
    (Ansi.hSupportsANSI stderr)
    (printErrorAnsi opts e)
    (printErrorText opts e)

runErrorIO ::
  (ToGenericError a, Member (Embed IO) r) =>
  GenericOptions ->
  Sem (Error a ': r) b ->
  Sem r b
runErrorIO opts =
  runError >=> \case
    Left err -> embed (printErrorAnsiSafe opts err >> exitFailure)
    Right a -> return a
