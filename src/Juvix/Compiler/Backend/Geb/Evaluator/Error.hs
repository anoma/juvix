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
    _evalErrorGebExpression :: !(Maybe Morphism)
  }

makeLenses ''EvalError

instance Exception.Exception EvalError

instance Show EvalError where
  show :: EvalError -> String
  show (EvalError {..}) =
    "evaluation error: "
      <> fromText _evalErrorMsg
      <> case _evalErrorGebValue of
        Nothing -> ""
        Just val -> "Value: " <> fromText (ppTrace val)
      <> case _evalErrorGebExpression of
        Nothing -> ""
        Just expr ->
          "Morphism:\n"
            <> fromText (Geb.ppTrace expr)

evalError :: Text -> Maybe GebValue -> Maybe Morphism -> a
evalError msg val m =
  Exception.throw
    ( EvalError
        { _evalErrorMsg = msg,
          _evalErrorGebValue = val,
          _evalErrorGebExpression = m
        }
    )
