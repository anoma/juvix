module Juvix.Compiler.Backend.Geb.Evaluator.Error where

import GHC.Exception qualified as Exception
import GHC.Show qualified as S
import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty

data EvalError = EvalError
  { _evalErrorMsg :: !Text,
    _evalErrorGebValue :: !(Maybe GebValue),
    _evalErrorGebExpression :: !(Maybe Morphism)
  }

data QuoteError = QuoteError
  { _quoteErrorMsg :: Text,
    _quoteErrorGebValue :: Maybe GebValue,
    _quoteErrorGebExpression :: Maybe Object
  }

makeLenses ''EvalError
makeLenses ''QuoteError

-- TODO: Make this a proper error with a location
instance ToGenericError EvalError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = defaultLoc,
          _genericErrorMessage = mkAnsiText (pack (S.show e)),
          _genericErrorIntervals = []
        }

mockFile :: Path Abs File
mockFile = $(mkAbsFile "/geb-eval-error")

defaultLoc :: Interval
defaultLoc = singletonInterval (mkInitialLoc mockFile)

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError {..}) =
    "evaluation error: "
      <> fromText _evalErrorMsg
      <> "\n"
      <> case _evalErrorGebValue of
        Nothing -> ""
        Just val -> "Value:\n" <> fromText (ppTrace val)
      <> "\n"
      <> case _evalErrorGebExpression of
        Nothing -> ""
        Just expr ->
          "Morphism:\n"
            <> fromText (ppTrace expr)
            <> "\n"

evalError ::
  (Member (Error JuvixError) r) =>
  Text ->
  Maybe GebValue ->
  Maybe Morphism ->
  Sem r a
evalError msg val m =
  throw . JuvixError $
    ( EvalError
        { _evalErrorMsg = msg,
          _evalErrorGebValue = val,
          _evalErrorGebExpression = m
        }
    )

instance Exception.Exception QuoteError

instance Show QuoteError where
  show :: QuoteError -> String
  show QuoteError {..} =
    "Quote error: "
      <> fromText _quoteErrorMsg
      <> case _quoteErrorGebValue of
        Nothing -> ""
        Just val -> ": " <> fromText (ppTrace val)
      <> case _quoteErrorGebExpression of
        Nothing -> ""
        Just expr ->
          "GebObject associated:\n"
            <> fromText (ppTrace expr)
