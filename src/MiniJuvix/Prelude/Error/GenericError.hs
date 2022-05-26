module MiniJuvix.Prelude.Error.GenericError
  ( module MiniJuvix.Prelude.Error.GenericError,
  )
where

import MiniJuvix.Prelude.Base
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Loc
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text
import System.Console.ANSI qualified as Ansi

data GenericError = GenericError
  { _genericErrorLoc :: Loc,
    _genericErrorFile :: FilePath,
    _genericErrorMessage :: AnsiText,
    _genericErrorIntervals :: [Interval]
  }

makeLenses ''GenericError

instance Pretty GenericError where
  pretty :: GenericError -> Doc a
  pretty g =
    let lineNum = g ^. genericErrorLoc . locFileLoc . locLine
        colNum = g ^. genericErrorLoc . locFileLoc . locCol
     in pretty (g ^. genericErrorFile)
          <> colon
          <> pretty lineNum
          <> colon
          <> pretty colNum
          <> colon <+> "error"
          <> colon
          <> line
          <> pretty (g ^. genericErrorMessage)

class ToGenericError a where
  genericError :: a -> GenericError

errorIntervals :: ToGenericError e => e -> [Interval]
errorIntervals = (^. genericErrorIntervals) . genericError

render :: ToGenericError e => Bool -> e -> Text
render ansi err
  | ansi = helper Ansi.renderStrict (toAnsiDoc (g ^. genericErrorMessage))
  | otherwise = helper renderStrict (toTextDoc (g ^. genericErrorMessage))
  where
    helper :: (SimpleDocStream a -> Text) -> Doc a -> Text
    helper f x = (f . layoutPretty defaultLayoutOptions) (header <> x <> endChar)
    g :: GenericError
    g = genericError err

    header :: Doc a
    header =
      let lineNum = g ^. genericErrorLoc . locFileLoc . locLine
          colNum = g ^. genericErrorLoc . locFileLoc . locCol
       in pretty (g ^. genericErrorFile)
            <> colon
            <> pretty lineNum
            <> colon
            <> pretty colNum
            <> colon <+> "error"
            <> colon
            <> line
    endChar :: Doc a
    endChar = "ת"

-- | Render the error to Text.
renderText :: ToGenericError e => e -> Text
renderText = render False

-- | Render the error with Ansi formatting (if any).
renderAnsiText :: ToGenericError e => e -> Text
renderAnsiText = render True

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
