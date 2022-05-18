module MiniJuvix.Prelude.Error.GenericError where

import MiniJuvix.Prelude.Base
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Concrete.Loc
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text

data GenericError = GenericError
  { _genericErrorLoc :: Loc,
    _genericErrorFile :: FilePath,
    _genericErrorMessage :: AnsiText,
    _genericErrorIntervals :: [Interval]
  }

makeLenses ''GenericError

class ToGenericError a where
  genericError :: a -> Maybe GenericError

instance ToGenericError Text where
  genericError = const Nothing

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

errorIntervals :: ToGenericError e => e -> [Interval]
errorIntervals = maybe [] (^. genericErrorIntervals) . genericError

renderGenericError :: Bool -> GenericError -> Text
renderGenericError ansi g
  | ansi = Ansi.renderStrict (layoutPretty defaultLayoutOptions (header <> toAnsiDoc (g ^. genericErrorMessage)))
  | otherwise = renderStrict (layoutPretty defaultLayoutOptions (header <> toTextDoc (g ^. genericErrorMessage)))
  where
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
