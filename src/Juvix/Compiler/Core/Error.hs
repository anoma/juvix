module Juvix.Compiler.Core.Error where

import GHC.Show
import Juvix.Compiler.Core.Language
import Juvix.Compiler.Core.Pretty
import Juvix.Parser.Error.Base

data CoreError = CoreError
  { _coreErrorMsg :: AnsiText,
    _coreErrorNode :: Maybe Node,
    _coreErrorLoc :: Location
  }

makeLenses ''CoreError

instance FromSimpleParserError CoreError where
  fromSimpleParserError e = CoreError (mkAnsiText (e ^. simpleParserErrorMessage)) Nothing (getLoc e)

instance ToGenericError CoreError where
  genericError e = ask >>= generr
    where
      generr opts =
        return
          GenericError
            { _genericErrorLoc = i,
              _genericErrorMessage = msg,
              _genericErrorIntervals = [i]
            }
        where
          i = getLoc e
          opts' = fromGenericOptions opts
          msg = case e ^. coreErrorNode of
            Just node -> ppOutput (pretty (e ^. coreErrorMsg) <> colon <> space <> doc opts' node)
            Nothing -> e ^. coreErrorMsg

instance Pretty CoreError where
  pretty CoreError {..} = case _coreErrorNode of
    Just node -> pretty _coreErrorMsg <> colon <> space <> pretty (ppTrace node)
    Nothing -> pretty _coreErrorMsg

instance Show CoreError where
  show e = fromText (prettyText e)

instance HasLoc CoreError where
  getLoc CoreError {..} = _coreErrorLoc
