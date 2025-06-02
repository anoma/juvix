module Juvix.Parser.Error.Base where

import Juvix.Prelude
import Juvix.Prelude.Pretty
import Text.Megaparsec qualified as M
import Text.Megaparsec.Error (errorOffset)

instance Pretty MegaparsecError where
  pretty (MegaparsecError b) = pretty (M.errorBundlePrettyWith (\_ _ -> M.parseErrorTextPretty) b)

instance HasLoc MegaparsecError where
  getLoc (MegaparsecError b) = singletonInterval (mkLoc offset sourcePos)
    where
      state :: M.PosState Text
      state = M.bundlePosState b
      offset = errorOffset (head (M.bundleErrors b))

      sourcePos :: M.SourcePos
      sourcePos =
        (snd . head . fst)
          (M.attachSourcePos errorOffset (M.bundleErrors b) state)

newtype MegaparsecError = MegaparsecError
  { _megaParsecError :: M.ParseErrorBundle Text Void
  }
  deriving stock (Show)

instance ToGenericError MegaparsecError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = mkAnsiText $ pretty @_ @AnsiStyle e,
          _genericErrorIntervals = [i]
        }
    where
      i = getLoc e

-- | Use only for debugging
fromMegaParsecError :: Either MegaparsecError a -> a
fromMegaParsecError = \case
  Left e -> error (prettyText e)
  Right a -> a

data SimpleParserError = SimpleParserError
  { _simpleParserErrorLoc :: Interval,
    _simpleParserErrorMessage :: Text
  }
  deriving stock (Show)

makeLenses ''SimpleParserError

instance HasLoc SimpleParserError where
  getLoc SimpleParserError {..} = _simpleParserErrorLoc

instance ToGenericError SimpleParserError where
  genericError SimpleParserError {..} =
    return
      GenericError
        { _genericErrorLoc = _simpleParserErrorLoc,
          _genericErrorMessage = mkAnsiText _simpleParserErrorMessage,
          _genericErrorIntervals = [_simpleParserErrorLoc]
        }

class FromSimpleParserError a where
  fromSimpleParserError :: SimpleParserError -> a

instance FromSimpleParserError SimpleParserError where
  fromSimpleParserError = id
