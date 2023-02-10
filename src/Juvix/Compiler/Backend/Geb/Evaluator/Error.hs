module Juvix.Compiler.Backend.Geb.Evaluator.Error where

import Control.Exception qualified as Exception
import GHC.Show qualified as S
import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Values

data EvalError = EvalError
  { _evalErrorMsg :: !Text,
    _evalErrorGebValue :: !(Maybe GebValue),
    _evalErrorGebExpression :: !(Maybe Expression)
  }

makeLenses ''EvalError

instance Exception.Exception EvalError

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError {..}) =
    "evaluation error: "
      <> fromText _evalErrorMsg
      <> case _evalErrorGebValue of
        Nothing -> "(no value)"
        Just val -> ": " <> fromText (ppTrace val)
      <> case _evalErrorGebExpression of
        Nothing -> "(no geb expression)"
        Just expr ->
          "GebObject associated:\n"
            <> fromText (Geb.ppTrace expr)

evalError :: Text -> Maybe GebValue -> Maybe Expression -> a
evalError msg val gebExpr =
  Exception.throw
    ( EvalError
        { _evalErrorMsg = msg,
          _evalErrorGebValue = val,
          _evalErrorGebExpression = gebExpr
        }
    )
