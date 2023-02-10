module Juvix.Compiler.Backend.Geb.Translation.FromGebValue
  ( module Juvix.Compiler.Backend.Geb.Translation.FromGebValue,
  )
where

import Control.Exception qualified as Exception
import GHC.Show qualified as S
import Juvix.Compiler.Backend.Geb.Evaluator.Data.Env
import Juvix.Compiler.Backend.Geb.Evaluator.Data.Values
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Pretty.Values

data FromGebValueError = FromGebValueError
  { _fromGebValueErrorMsg :: !Text,
    _fromGebValueErrorGebValue :: !(Maybe GebValue),
    _fromGebValueErrorGebExpression :: !(Maybe Object)
  }

makeLenses ''FromGebValueError

instance Exception.Exception FromGebValueError

instance Show FromGebValueError where
  show :: FromGebValueError -> String
  show (FromGebValueError {..}) =
    "fromGebValue error: "
      <> fromText _fromGebValueErrorMsg
      <> case _fromGebValueErrorGebValue of
        Nothing -> "(no value)"
        Just val -> ": " <> fromText (ppTrace val)
      <> case _fromGebValueErrorGebExpression of
        Nothing -> "(no geb object)"
        Just expr ->
          "GebObject associated:\n"
            <> fromText (Geb.ppTrace expr)

fromGebValueError :: Text -> GebValue -> Object -> a
fromGebValueError msg val gebExpr =
  Exception.throw
    ( FromGebValueError
        { _fromGebValueErrorMsg = msg,
          _fromGebValueErrorGebValue = Just val,
          _fromGebValueErrorGebExpression = Just gebExpr
        }
    )

-- TODO: use fromGebValueError

-- | Quoting a GebValue to a Morphism.
-- Morphisms carry the type of the value they represent, in
-- contrast to GebValues, which do not. To quote a GebValue,
-- we need to know the type of the value.
fromGebValue ::
  Members '[Reader Env, Error JuvixError] r =>
  Object ->
  GebValue ->
  Sem r Morphism
fromGebValue ty = \case
  GebValueMorphismInteger i -> case ty of
    ObjectInteger -> return $ MorphismInteger i
    _ -> errorFromGebValue "type mismatch. Expected Integer"
  GebValueMorphismUnit -> case ty of
    ObjectTerminal -> return MorphismUnit
    _ -> errorFromGebValue "type mismatch. Expected Unit"
  GebValueMorphismLeft m -> case ty of
    ObjectCoproduct _ -> MorphismLeft <$> fromGebValue ty m
    _ -> errorFromGebValue "type mismatch (left). Expected a coproduct"
  GebValueMorphismRight m -> case ty of
    ObjectCoproduct _ -> MorphismRight <$> fromGebValue ty m
    _ -> errorFromGebValue "type mismatch (right). Expected a coproduct"
  GebValueMorphismPair m -> case ty of
    ObjectProduct prod -> do
      let (a, b) = (prod ^. productLeft, prod ^. productRight)
      pLeft <- fromGebValue a (m ^. valueMorphismPairLeft)
      pRight <- fromGebValue b (m ^. valueMorphismPairRight)
      return $
        MorphismPair
          Pair
            { _pairLeft = pLeft,
              _pairRight = pRight,
              _pairLeftType = a,
              _pairRightType = b
            }
    _ -> errorFromGebValue "type mismatch (pair). Expected a product"
  GebValueClosure cls -> return $ MorphismLambda $ cls ^. valueClosureLambda

errorFromGebValue :: Text -> a
errorFromGebValue = error . ("fromGebValue: " <>)
