module Juvix.Parser.Error where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.Megaparsec qualified as M
import Text.Megaparsec.Error (errorOffset)

newtype ParserError = ParserError
  { _parseError :: M.ParseErrorBundle Text Void
  }
  deriving stock (Show)

instance ToGenericError ParserError where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = AnsiText $ pretty @_ @AnsiStyle e,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc e

instance Pretty ParserError where
  pretty (ParserError b) = pretty (M.errorBundlePretty b)

instance HasLoc ParserError where
  getLoc (ParserError b) = singletonInterval (mkLoc "." offset sourcePos)
    where
      state :: M.PosState Text
      state = M.bundlePosState b
      offset = errorOffset (head (M.bundleErrors b))
      sourcePos :: M.SourcePos
      sourcePos =
        (snd . head . fst)
          (M.attachSourcePos errorOffset (M.bundleErrors b) state)
