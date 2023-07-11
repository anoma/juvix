module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
    module Juvix.Compiler.Backend.Geb.Evaluator.Data,
  )
where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Data
import Juvix.Compiler.Backend.Geb.Evaluator.Error
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Extra
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
  return $ quote val

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
    MorphismFail x -> evalFail x

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
      Pair
        { _pairLeft = left,
          _pairRight = right
        }

evalFirst :: EvalEffects r => First -> Sem r GebValue
evalFirst f = do
  res <- eval $ f ^. firstValue
  case res of
    GebValueMorphismPair pair -> return $ pair ^. pairLeft
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
    GebValueMorphismPair pair -> return $ pair ^. pairRight
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
  return $
    GebValueMorphismLeft $
      LeftInj
        { _leftInjValue = res,
          _leftInjRightType = s ^. leftInjRightType
        }

evalRightInj :: EvalEffects r => RightInj -> Sem r GebValue
evalRightInj s = do
  res <- eval $ s ^. rightInjValue
  return $
    GebValueMorphismRight $
      RightInj
        { _rightInjValue = res,
          _rightInjLeftType = s ^. rightInjLeftType
        }

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
        local (set envContext bodyEnv) $
          eval (cls ^. valueClosureLambda . lambdaBody)
    _ ->
      throw $
        EvalError
          { _evalErrorMsg = "Can only apply functions.",
            _evalErrorGebValue = (Just fun),
            _evalErrorGebExpression = Nothing
          }

evalExtendContext :: EvalEffects r => GebValue -> Morphism -> Sem r GebValue
evalExtendContext v m = do
  ctx <- asks (^. envContext)
  local (set envContext (Context.cons v ctx)) $
    eval m

evalLambda :: EvalEffects r => Lambda -> Sem r GebValue
evalLambda lambda = do
  ctx <- asks (^. envContext)
  return $
    GebValueClosure $
      ValueClosure
        { _valueClosureLambda = lambda,
          _valueClosureEnv = ctx
        }

evalCase :: EvalEffects r => Case -> Sem r GebValue
evalCase c = do
  vCaseOn <- eval $ c ^. caseOn
  case vCaseOn of
    GebValueMorphismLeft leftArg -> evalExtendContext (leftArg ^. leftInjValue) (c ^. caseLeft)
    GebValueMorphismRight rightArg -> evalExtendContext (rightArg ^. rightInjValue) (c ^. caseRight)
    _ ->
      throw
        EvalError
          { _evalErrorMsg = "Case can only be applied to terms of the coproduct object.",
            _evalErrorGebValue = Just vCaseOn,
            _evalErrorGebExpression = Just (MorphismCase c)
          }

evalBinop ::
  EvalEffects r =>
  Binop ->
  Sem r GebValue
evalBinop binop = do
  left <- eval $ binop ^. binopLeft
  right <- eval $ binop ^. binopRight
  let lfPair m1 m2 =
        ( GebValueMorphismPair
            ( Pair
                { _pairLeft = m1,
                  _pairRight = m2
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
              | l == r -> return valueTrue
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

evalFail :: EvalEffects r => Failure -> Sem r GebValue
evalFail Failure {..} =
  throw
    EvalError
      { _evalErrorMsg = "failure: " <> _failureMessage,
        _evalErrorGebValue = Nothing,
        _evalErrorGebExpression = Nothing
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
valueTrue =
  GebValueMorphismLeft $
    LeftInj
      { _leftInjValue = GebValueMorphismUnit,
        _leftInjRightType = ObjectTerminal
      }

valueFalse :: GebValue
valueFalse =
  GebValueMorphismRight $
    RightInj
      { _rightInjValue = GebValueMorphismUnit,
        _rightInjLeftType = ObjectTerminal
      }

quote :: GebValue -> Morphism
quote = \case
  GebValueClosure cls -> quoteClosure cls
  GebValueMorphismInteger i -> MorphismInteger i
  GebValueMorphismLeft m -> quoteValueMorphismLeft m
  GebValueMorphismPair m -> quoteValueMorphismPair m
  GebValueMorphismRight m -> quoteValueMorphismRight m
  GebValueMorphismUnit -> MorphismUnit

quoteClosure :: ValueClosure -> Morphism
quoteClosure cls =
  let env = map quote (toList (cls ^. valueClosureEnv))
   in substs env (MorphismLambda (cls ^. valueClosureLambda))

quoteValueMorphismPair :: ValuePair -> Morphism
quoteValueMorphismPair vpair =
  let pLeft = quote (vpair ^. pairLeft)
      pRight = quote (vpair ^. pairRight)
   in MorphismPair
        Pair
          { _pairLeft = pLeft,
            _pairRight = pRight
          }

quoteValueMorphismLeft :: ValueLeftInj -> Morphism
quoteValueMorphismLeft m =
  let leftMorphism = quote (m ^. leftInjValue)
   in MorphismLeft
        LeftInj
          { _leftInjValue = leftMorphism,
            _leftInjRightType = m ^. leftInjRightType
          }

quoteValueMorphismRight :: ValueRightInj -> Morphism
quoteValueMorphismRight m =
  let rightMorphism = quote (m ^. rightInjValue)
   in MorphismRight
        RightInj
          { _rightInjValue = rightMorphism,
            _rightInjLeftType = m ^. rightInjLeftType
          }
