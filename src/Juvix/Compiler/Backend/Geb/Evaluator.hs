module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
    module Juvix.Compiler.Backend.Geb.Evaluator.Data,
  )
where

import Control.DeepSeq
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Data
import Juvix.Compiler.Backend.Geb.Evaluator.Error
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Translation.FromGebValue
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
eval' env m = run . runError $ runReader env (eval m)

nf' :: Env -> Morphism -> Either JuvixError Morphism
nf' env m = run . runError $ runReader env (nf m)

nf ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r Morphism
nf m = do
  val :: GebValue <- eval m
  obj :: Object <- runReader defaultInferenceEnv (infer m)
  if
      | needObjectInfo val -> fromGebValue (Just obj) val
      | otherwise -> fromGebValue Nothing val

eval ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r GebValue
eval morph = case morph of
  MorphismAbsurd _ ->
    evalError "Absurd can not be evaluated." Nothing (Just morph)
  MorphismVar var -> do
    ctx <- asks (^. envContext)
    return $ Context.lookup (var ^. varIndex) ctx
  MorphismUnit -> return GebValueMorphismUnit
  MorphismInteger i -> return $ GebValueMorphismInteger i
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
        evalError
          "First can only be applied to pairs."
          Nothing
          (Just morph)
  MorphismSecond s -> do
    res <- eval $ s ^. secondValue
    case res of
      GebValueMorphismPair pair ->
        return $ pair ^. valueMorphismPairRight
      _ ->
        evalError
          "Second can only be applied to pairs."
          (Just res)
          (Just morph)
  MorphismBinop op -> applyBinop op
  MorphismApplication app ->
    apply (app ^. applicationLeft) (app ^. applicationRight)
  MorphismLambda lambda -> do
    ctx <- asks (^. envContext)
    return $
      GebValueClosure $
        ValueClosure
          { _valueClosureLambda = lambda,
            _valueClosureEnv = ctx
          }
  MorphismLeft m -> GebValueMorphismLeft <$> eval m
  MorphismRight m -> GebValueMorphismRight <$> eval m
  MorphismCase c -> do
    vCaseOn <- eval $ c ^. caseOn
    case vCaseOn of
      GebValueMorphismLeft leftArg -> do
        let fun' =
              MorphismLambda
                Lambda
                  { _lambdaBody = c ^. caseLeft,
                    _lambdaVarType = c ^. caseLeftType,
                    _lambdaBodyType = c ^. caseCodomainType
                  }
        fun <- eval fun'
        apply' fun leftArg
      GebValueMorphismRight rightArg -> do
        let fun' =
              MorphismLambda
                Lambda
                  { _lambdaBody = c ^. caseRight,
                    _lambdaVarType = c ^. caseRightType,
                    _lambdaBodyType = c ^. caseCodomainType
                  }
        fun <- eval fun'
        apply' fun rightArg
      _ ->
        evalError
          "Case can only be applied to terms of the coproduct object."
          (Just vCaseOn)
          (Just morph)

apply ::
  Members '[Reader Env, Error JuvixError] r =>
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
      local (over envContext (Context.cons arg)) $
        eval (cls ^. valueClosureLambda . lambdaBody)
    _ -> evalError "Can only apply functions." (Just fun) (Just fun')

apply' ::
  Members '[Reader Env, Error JuvixError] r =>
  GebValue ->
  GebValue ->
  Sem r GebValue
apply' fun arg =
  case fun of
    GebValueClosure cls ->
      local (over envContext (Context.cons arg)) $
        eval (cls ^. valueClosureLambda . lambdaBody)
    _ -> evalError "Can only apply functions." (Just fun) Nothing

applyBinop ::
  Members '[Reader Env, Error JuvixError] r =>
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
                evalError
                  "Equality can only be applied to values of the same kind."
                  (Just (lfPair m1 m2))
                  (Just (MorphismBinop binop))
      _ ->
        evalError
          "Canot apply operation"
          (Just (lfPair m1 m2))
          (Just (MorphismBinop binop))

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
