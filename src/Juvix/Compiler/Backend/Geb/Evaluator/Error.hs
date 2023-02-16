module Juvix.Compiler.Backend.Geb.Evaluator.Error where

import GHC.Show qualified as S
import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Values

data EvalError = EvalError
  { _evalErrorMsg :: !Text,
    _evalErrorGebValue :: !(Maybe GebValue),
    _evalErrorGebExpression :: !(Maybe Morphism)
  }

makeLenses ''EvalError

-- TODO: Make this a proper error with a location
instance ToGenericError EvalError where
  genericError e =
    return
      GenericError
        { _genericErrorLoc = defaultLoc,
          _genericErrorMessage = AnsiText (pack $ S.show e),
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
            <> fromText (Geb.ppTrace expr)
            <> "\n"

evalError ::
  Member (Error JuvixError) r =>
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
