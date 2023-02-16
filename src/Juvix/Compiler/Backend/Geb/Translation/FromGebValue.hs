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
  { _fromGebValueErrorMsg :: Text,
    _fromGebValueErrorGebValue :: Maybe GebValue,
    _fromGebValueErrorGebExpression :: Maybe Object
  }

makeLenses ''FromGebValueError

instance Exception.Exception FromGebValueError

instance Show FromGebValueError where
  show :: FromGebValueError -> String
  show FromGebValueError {..} =
    "fromGebValue error: "
      <> fromText _fromGebValueErrorMsg
      <> case _fromGebValueErrorGebValue of
        Nothing -> ""
        Just val -> ": " <> fromText (ppTrace val)
      <> case _fromGebValueErrorGebExpression of
        Nothing -> ""
        Just expr ->
          "GebObject associated:\n"
            <> fromText (Geb.ppTrace expr)

fromGebValueError :: Text -> Maybe GebValue -> Maybe Object -> a
fromGebValueError msg val gebExpr =
  Exception.throw
    FromGebValueError
      { _fromGebValueErrorMsg = msg,
        _fromGebValueErrorGebValue = val,
        _fromGebValueErrorGebExpression = gebExpr
      }

-- We cannot quote GebValues to Morphisms without knowing the type
-- in some cases. For example, we cannot quote a GebValueMorphismLeft.
-- So we need to provide the Geb object associated to the Geb value.

needObjectInfo :: GebValue -> Bool
needObjectInfo = \case
  GebValueMorphismUnit -> False
  GebValueMorphismInteger {} -> False
  GebValueClosure {} -> False
  GebValueMorphismLeft {} -> True
  GebValueMorphismRight {} -> True
  GebValueMorphismPair {} -> True

-- | Quote GebValues to Morphisms.
fromGebValue ::
  Members '[Reader Env, Error JuvixError] r =>
  Maybe Object ->
  GebValue ->
  Sem r Morphism
fromGebValue ty = \case
  GebValueMorphismInteger i -> return $ MorphismInteger i
  GebValueMorphismUnit -> return MorphismUnit
  GebValueClosure cls -> case ty of
    -- FIXME: body <- apply' fun (var' not in the env)
    -- expr <- fromGebvalue  bodyCal with env extended with var'
    -- return lambda body.
    Just (ObjectHom funObj) ->
      return $
        MorphismLambda
          Lambda
            { _lambdaVarType = funObj ^. homDomain,
              _lambdaBodyType = funObj ^. homCodomain,
              _lambdaBody =
                cls ^. valueClosureLambdaBody
            }
    Just _ ->
      fromGebValueError
        "Got Helper wrong object. Expected function object (lambda)"
        Nothing
        ty
    Nothing -> fromGebValueError "(closure) Need object info" Nothing ty
  val@(GebValueMorphismLeft m) -> case ty of
    Just (ObjectCoproduct _) -> MorphismLeft <$> fromGebValue ty m
    Just _ ->
      fromGebValueError
        "type mismatch (left). Expected a coproduct"
        (Just val)
        ty
    Nothing -> fromGebValueError "need object info" (Just val) ty
  val@(GebValueMorphismRight m) -> case ty of
    Just (ObjectCoproduct _) -> MorphismRight <$> fromGebValue ty m
    Just _ ->
      fromGebValueError
        "type mismatch (right). Expected a coproduct"
        (Just val)
        ty
    Nothing -> fromGebValueError "need object info" (Just val) ty
  val@(GebValueMorphismPair m) -> case ty of
    Just (ObjectProduct prod) -> do
      let (a, b) = (prod ^. productLeft, prod ^. productRight)
      pLeft <- fromGebValue (Just a) (m ^. valueMorphismPairLeft)
      pRight <- fromGebValue (Just b) (m ^. valueMorphismPairRight)
      return $
        MorphismPair
          Pair
            { _pairLeft = pLeft,
              _pairRight = pRight,
              _pairLeftType = a,
              _pairRightType = b
            }
    Just _ ->
      fromGebValueError
        "type mismatch (pair). Expected a product"
        (Just val)
        ty
    Nothing -> fromGebValueError "need object info" (Just val) ty
