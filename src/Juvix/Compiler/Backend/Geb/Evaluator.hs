module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
    module Juvix.Compiler.Backend.Geb.Evaluator.Data,
  )
where

import Control.Exception qualified as Exception
import Juvix.Compiler.Backend.Geb.Analysis.TypeChecking as Geb
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Data
import Juvix.Compiler.Backend.Geb.Evaluator.Error
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Translation.FromSource as Geb

data RunEvalArgs = RunEvalArgs
  { _runEvalArgsInputFile :: Path Abs File,
    _runEvalArgsContent :: Text,
    _runEvalArgsEvaluatorOptions :: EvaluatorOptions
  }

makeLenses ''RunEvalArgs

runEval :: RunEvalArgs -> Either JuvixError RunEvalResult
runEval RunEvalArgs {..} = do
  case Geb.runParser _runEvalArgsInputFile _runEvalArgsContent of
    Left err -> Left (JuvixError err)
    Right m' -> do
      let env :: Env =
            Env
              { _envEvaluatorOptions = _runEvalArgsEvaluatorOptions,
                _envContext = mempty
              }
      let outputFormat morph
            | _runEvalArgsEvaluatorOptions ^. evaluatorOptionsOutputMorphism =
                RunEvalResultMorphism <$> evalAndOutputMorphism' env morph
            | otherwise = RunEvalResultGebValue <$> eval' env morph

      case m' of
        ExpressionObject _ -> Left (error @JuvixError objNoEvalMsg)
        ExpressionTypedMorphism tyMorph ->
          outputFormat (tyMorph ^. typedMorphism)
        ExpressionMorphism m -> outputFormat m

objNoEvalMsg :: Text
objNoEvalMsg = "Geb objects cannot be evaluated, only morphisms."

eval' :: Env -> Morphism -> Either JuvixError GebValue
eval' env m =
  run . runError $
    mapError (JuvixError @EvalError) $
      runReader env $
        eval m

evalAndOutputMorphism' :: Env -> Morphism -> Either JuvixError Morphism
evalAndOutputMorphism' env m = run . runError $ runReader env (evalAndOutputMorphism m)

evalAndOutputMorphism ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r Morphism
evalAndOutputMorphism m = do
  val :: GebValue <- mapError (JuvixError @EvalError) $ eval m
  obj :: Object <-
    runReader (mempty @CheckingEnv) $
      mapError (JuvixError @CheckingError) (inferObject m)
  if
      | requiresObjectInfo val -> quote (Just obj) val
      | otherwise -> quote Nothing val

type EvalEffects r = Members '[Reader Env, Error EvalError] r

eval :: EvalEffects r => Morphism -> Sem r GebValue
eval morph =
  case morph of
    MorphismAbsurd x -> evalAbsurd x
    MorphismApplication app -> evalApp app
    MorphismBinop op -> evalBinop op
    MorphismCase c -> evalCase c
    MorphismFirst f -> evalFirst f
    MorphismInteger i -> return $ GebValueMorphismInteger i
    MorphismLambda l -> evalLambda l
    MorphismLeft m -> evalLeftInj m
    MorphismPair p -> evalPair p
    MorphismRight m -> evalRightInj m
    MorphismSecond s -> evalSecond s
    MorphismUnit -> return GebValueMorphismUnit
    MorphismVar x -> evalVar x

evalVar :: EvalEffects r => Var -> Sem r GebValue
evalVar var = do
  ctx <- asks (^. envContext)
  let val = Context.lookup (var ^. varIndex) ctx
  return val

evalAbsurd :: EvalEffects r => Absurd -> Sem r GebValue
evalAbsurd morph =
  throw
    EvalError
      { _evalErrorMsg = "Absurd can not be evaluated.",
        _evalErrorGebValue = Nothing,
        _evalErrorGebExpression = Just $ MorphismAbsurd morph
      }

evalPair :: EvalEffects r => Pair -> Sem r GebValue
evalPair pair = do
  left <- eval $ pair ^. pairLeft
  right <- eval $ pair ^. pairRight
  return $
    GebValueMorphismPair $
      ValueMorphismPair
        { _valueMorphismPairLeft = left,
          _valueMorphismPairRight = right
        }

evalFirst :: EvalEffects r => First -> Sem r GebValue
evalFirst f = do
  res <- eval $ f ^. firstValue
  case res of
    GebValueMorphismPair pair -> return $ pair ^. valueMorphismPairLeft
    _ ->
      throw
        EvalError
          { _evalErrorMsg = "First can only be applied to pairs.",
            _evalErrorGebValue = Nothing,
            _evalErrorGebExpression = Just (MorphismFirst f)
          }

evalSecond :: EvalEffects r => Second -> Sem r GebValue
evalSecond s = do
  res <- eval $ s ^. secondValue
  case res of
    GebValueMorphismPair pair -> return $ pair ^. valueMorphismPairRight
    _ ->
      throw
        EvalError
          { _evalErrorMsg = "Second can only be applied to pairs.",
            _evalErrorGebValue = Just res,
            _evalErrorGebExpression = Just (MorphismSecond s)
          }

evalLeftInj :: EvalEffects r => LeftInj -> Sem r GebValue
evalLeftInj s = do
  res <- eval $ s ^. leftInjValue
  return $ GebValueMorphismLeft res

evalRightInj :: EvalEffects r => RightInj -> Sem r GebValue
evalRightInj s = do
  res <- eval $ s ^. rightInjValue
  return $ GebValueMorphismRight res

evalApp :: EvalEffects r => Application -> Sem r GebValue
evalApp app = do
  arg <- eval (app ^. applicationRight)
  apply (app ^. applicationLeft) arg

apply ::
  EvalEffects r =>
  Morphism ->
  GebValue ->
  Sem r GebValue
apply fun' arg = do
  fun <- eval fun'
  case fun of
    GebValueClosure cls ->
      do
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

evalLambda :: EvalEffects r => Lambda -> Sem r GebValue
evalLambda lambda = do
  ctx <- asks (^. envContext)
  return $
    GebValueClosure $
      ValueClosure
        { _valueClosureLambdaBody = lambda ^. lambdaBody,
          _valueClosureEnv = ctx
        }

evalCase :: EvalEffects r => Case -> Sem r GebValue
evalCase c = do
  vCaseOn <- eval $ c ^. caseOn
  case vCaseOn of
    GebValueMorphismLeft leftArg -> apply (c ^. caseLeft) leftArg
    GebValueMorphismRight rightArg -> apply (c ^. caseRight) rightArg
    _ ->
      throw
        EvalError
          { _evalErrorMsg = "Case can only be applied to terms of the coproduct object.",
            _evalErrorGebValue = Just vCaseOn,
            _evalErrorGebExpression = Just (MorphismCase c)
          }

evalBinop ::
  Members '[Reader Env, Error EvalError] r =>
  Binop ->
  Sem r GebValue
evalBinop binop = do
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
            { _evalErrorMsg = "Cannot apply operation",
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

requiresObjectInfo :: GebValue -> Bool
requiresObjectInfo = \case
  GebValueMorphismUnit -> False
  GebValueMorphismInteger {} -> False
  GebValueClosure {} -> True
  GebValueMorphismLeft {} -> False
  GebValueMorphismRight {} -> False
  GebValueMorphismPair {} -> True

quote :: Maybe Object -> GebValue -> Sem r Morphism
quote ty = \case
  GebValueClosure cls -> quoteClosure ty cls
  GebValueMorphismInteger i -> return $ MorphismInteger i
  GebValueMorphismLeft m -> quoteValueMorphismLeft ty m
  GebValueMorphismPair m -> quoteValueMorphismPair ty m
  GebValueMorphismRight m -> quoteMorphismRight ty m
  GebValueMorphismUnit -> return MorphismUnit

quoteClosure :: Maybe Object -> ValueClosure -> Sem r Morphism
quoteClosure ty cls =
  quoteError
    "Not implemented yet"
    (Just (GebValueClosure cls))
    ty

quoteValueMorphismPair :: Maybe Object -> ValueMorphismPair -> Sem r Morphism
quoteValueMorphismPair ty vpair = do
  case ty of
    Just (ObjectProduct prod) -> do
      let (a, b) = (prod ^. productLeft, prod ^. productRight)
      pLeft <- quote (Just a) (vpair ^. valueMorphismPairLeft)
      pRight <- quote (Just b) (vpair ^. valueMorphismPairRight)
      return $
        MorphismPair
          Pair
            { _pairLeft = pLeft,
              _pairRight = pRight,
              _pairLeftType = a,
              _pairRightType = b
            }
    Just _ ->
      quoteError
        "type mismatch (pair). Expected a product"
        (Just (GebValueMorphismPair vpair))
        ty
    Nothing ->
      quoteError
        "need object info"
        (Just (GebValueMorphismPair vpair))
        ty

quoteValueMorphismLeft :: Maybe Object -> GebValue -> Sem r Morphism
quoteValueMorphismLeft ty m = case ty of
  Just (ObjectCoproduct Coproduct {..}) -> do
    leftMorphism <- quote ty m
    return $
      MorphismLeft
        LeftInj
          { _leftInjValue = leftMorphism,
            _leftInjLeftType = _coproductLeft,
            _leftInjRightType = _coproductRight
          }
  Just _ ->
    quoteError
      "type mismatch (left). Expected a coproduct"
      (Just (GebValueMorphismLeft m))
      ty
  Nothing -> quoteError "need object info" (Just (GebValueMorphismLeft m)) ty

quoteMorphismRight :: Maybe Object -> GebValue -> Sem r Morphism
quoteMorphismRight ty r = case ty of
  Just (ObjectCoproduct Coproduct {..}) -> do
    rightMorphism <- quote ty r
    return $
      MorphismRight
        RightInj
          { _rightInjValue = rightMorphism,
            _rightInjLeftType = _coproductLeft,
            _rightInjRightType = _coproductRight
          }
  Just _ ->
    quoteError
      "type mismatch (right). Expected a coproduct"
      (Just (GebValueMorphismRight r))
      ty
  Nothing -> quoteError "need object info" (Just (GebValueMorphismRight r)) ty

quoteError :: Text -> Maybe GebValue -> Maybe Object -> a
quoteError msg val gebExpr =
  Exception.throw
    QuoteError
      { _quoteErrorMsg = msg,
        _quoteErrorGebValue = val,
        _quoteErrorGebExpression = gebExpr
      }
