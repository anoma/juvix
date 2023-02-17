module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
    module Juvix.Compiler.Backend.Geb.Evaluator.Data,
  )
where

import Control.DeepSeq
import Control.Exception qualified as Exception
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Data
import Juvix.Compiler.Backend.Geb.Evaluator.Error
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Translation.FromSource as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking as Geb

data RunEvalArgs = RunEvalArgs
  { _runEvalArgsInputFile :: Path Abs File,
    _runEvalArgsContent :: Text,
    _runEvalArgsEvaluatorOptions :: EvaluatorOptions
  }

makeLenses ''RunEvalArgs

runEval :: RunEvalArgs -> Either JuvixError RunEvalResult
runEval RunEvalArgs {..} =
  case Geb.runParser _runEvalArgsInputFile _runEvalArgsContent of
    Right (ExpressionMorphism m) -> do
      let env :: Env =
            Env
              { _envEvaluatorOptions = _runEvalArgsEvaluatorOptions,
                _envContext = Context.empty
              }
      if _runEvalArgsEvaluatorOptions ^. evaluatorOptionsNormalise
        then RunEvalResultMorphism <$> nf' env m
        else RunEvalResultGebValue <$> eval' env m
    Right _ -> Left (error @JuvixError objNoEvalMsg)
    Left err -> Left (JuvixError err)

objNoEvalMsg :: Text
objNoEvalMsg = "Geb objects cannot be evaluated, only morphisms."

eval' :: Env -> Morphism -> Either JuvixError GebValue
eval' env m =
  run . runError $
    mapError (JuvixError @EvalError) $
      runReader env $
        eval m

nf' :: Env -> Morphism -> Either JuvixError Morphism
nf' env m = run . runError $ runReader env (nf m)

nf ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r Morphism
nf m = do
  val :: GebValue <- mapError (JuvixError @EvalError) $ eval m
  obj :: Object <- runReader defaultInferenceEnv (infer m)
  if
      | needObjectInfo val -> fromGebValue (Just obj) val
      | otherwise -> fromGebValue Nothing val

eval ::
  Members '[Reader Env, Error EvalError] r =>
  Morphism ->
  Sem r GebValue
eval morph = case morph of
  MorphismVar var -> do
    ctx <- asks (^. envContext)
    return $ Context.lookup (var ^. varIndex) ctx
  MorphismAbsurd _ ->
    throw
      EvalError
        { _evalErrorMsg = "Absurd can not be evaluated.",
          _evalErrorGebValue = Nothing,
          _evalErrorGebExpression = Just morph
        }
  MorphismUnit -> return GebValueMorphismUnit
  MorphismInteger i -> return $ GebValueMorphismInteger i
  MorphismBinop op -> applyBinop op
  MorphismPair pair -> do
    left <- eval $ pair ^. pairLeft
    right <- eval $ pair ^. pairRight
    return $
      GebValueMorphismPair $
        ValueMorphismPair
          { _valueMorphismPairLeft = left,
            _valueMorphismPairRight = right
          }
  MorphismFirst f -> do
    res <- eval $ f ^. firstValue
    case res of
      GebValueMorphismPair pair ->
        return $ pair ^. valueMorphismPairLeft
      _ ->
        throw
          EvalError
            { _evalErrorMsg = "First can only be applied to pairs.",
              _evalErrorGebValue = Nothing,
              _evalErrorGebExpression = Just morph
            }
  MorphismSecond s -> do
    res <- eval $ s ^. secondValue
    case res of
      GebValueMorphismPair pair ->
        return $ pair ^. valueMorphismPairRight
      _ ->
        throw
          EvalError
            { _evalErrorMsg = "Second can only be applied to pairs.",
              _evalErrorGebValue = Just res,
              _evalErrorGebExpression = Just morph
            }
  MorphismApplication app ->
    apply (app ^. applicationLeft) (app ^. applicationRight)
  MorphismLambda lambda -> do
    ctx <- asks (^. envContext)
    return $
      GebValueClosure $
        ValueClosure
          { _valueClosureLambdaBody = lambda ^. lambdaBody,
            _valueClosureEnv = ctx
          }
  MorphismLeft m -> GebValueMorphismLeft <$> eval m
  MorphismRight m -> GebValueMorphismRight <$> eval m
  MorphismCase c -> do
    vCaseOn <- eval $ c ^. caseOn
    case vCaseOn of
      GebValueMorphismLeft leftArg -> apply' (c ^. caseLeft) leftArg
      GebValueMorphismRight rightArg -> apply' (c ^. caseRight) rightArg
      _ ->
        throw
          EvalError
            { _evalErrorMsg = "Case can only be applied to terms of the coproduct object.",
              _evalErrorGebValue = Just vCaseOn,
              _evalErrorGebExpression = Just morph
            }

apply ::
  Members '[Reader Env, Error EvalError] r =>
  Morphism ->
  Morphism ->
  Sem r GebValue
apply fun' arg' = do
  evalStrategy <- asks (^. envEvaluatorOptions . evaluatorOptionsEvalStrategy)
  let maybeForce :: GebValue -> GebValue
      maybeForce = case evalStrategy of
        CallByName -> id
        CallByValue -> force
  arg <- maybeForce <$> eval arg'
  fun <- eval fun'
  case fun of
    GebValueClosure cls ->
      do
        let clsEnv = cls ^. valueClosureEnv
            bodyEnv = Context.cons arg clsEnv
        local (over envContext (const bodyEnv)) $
          eval (cls ^. valueClosureLambdaBody)
    _ ->
      throw
        EvalError
          { _evalErrorMsg = "Can only apply functions.",
            _evalErrorGebValue = Just fun,
            _evalErrorGebExpression = Just fun'
          }

apply' ::
  Members '[Reader Env, Error EvalError] r =>
  Morphism ->
  GebValue ->
  Sem r GebValue
apply' fun' arg = do
  fun <- eval fun'
  case fun of
    GebValueClosure cls -> do
      let clsEnv = cls ^. valueClosureEnv
          bodyEnv = Context.cons arg clsEnv
      local (over envContext (const bodyEnv)) $
        eval (cls ^. valueClosureLambdaBody)
    _ ->
      throw $
        EvalError
          { _evalErrorMsg = "Can only apply functions.",
            _evalErrorGebValue = (Just fun),
            _evalErrorGebExpression = Nothing
          }

applyBinop ::
  Members '[Reader Env, Error EvalError] r =>
  Binop ->
  Sem r GebValue
applyBinop binop = do
  left <- eval $ binop ^. binopLeft
  right <- eval $ binop ^. binopRight
  let lfPair m1 m2 =
        ( GebValueMorphismPair
            ( ValueMorphismPair
                { _valueMorphismPairLeft = m1,
                  _valueMorphismPairRight = m2
                }
            )
        )
  case (left, right) of
    (GebValueMorphismInteger l, GebValueMorphismInteger r) ->
      case binop ^. binopOpcode of
        OpAdd -> return $ GebValueMorphismInteger $ l + r
        OpSub -> return $ GebValueMorphismInteger $ l - r
        OpMul -> return $ GebValueMorphismInteger $ l * r
        OpDiv -> return $ GebValueMorphismInteger $ l `div` r
        OpMod -> return $ GebValueMorphismInteger $ l `mod` r
        OpLt ->
          if
              | l < r -> return valueTrue
              | otherwise -> return valueFalse
        OpEq ->
          if
              | l < r -> return valueTrue
              | otherwise -> return valueFalse
    (m1, m2) -> case binop ^. binopOpcode of
      OpEq ->
        if
            | sameKind m1 m2 ->
                if
                    | m1 == m2 -> return valueTrue
                    | otherwise -> return valueFalse
            | otherwise ->
                throw
                  EvalError
                    { _evalErrorMsg = "Equality can only be applied to values of the same kind.",
                      _evalErrorGebValue = Just (lfPair m1 m2),
                      _evalErrorGebExpression = (Just (MorphismBinop binop))
                    }
      _ ->
        throw
          EvalError
            { _evalErrorMsg = "Canot apply operation",
              _evalErrorGebValue = Just (lfPair m1 m2),
              _evalErrorGebExpression = Just (MorphismBinop binop)
            }

sameKind :: GebValue -> GebValue -> Bool
sameKind l r = case (l, r) of
  (GebValueMorphismInteger _, GebValueMorphismInteger _) -> True
  (GebValueMorphismUnit, GebValueMorphismUnit) -> True
  (GebValueMorphismLeft _, GebValueMorphismLeft _) -> True
  (GebValueMorphismRight _, GebValueMorphismRight _) -> True
  (GebValueMorphismPair _, GebValueMorphismPair _) -> True
  (GebValueClosure _, GebValueClosure _) -> True
  _ -> False

valueTrue :: GebValue
valueTrue = GebValueMorphismLeft GebValueMorphismUnit

valueFalse :: GebValue
valueFalse = GebValueMorphismRight GebValueMorphismUnit

-- TODO: DONT REVIEW BELOW YET
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
