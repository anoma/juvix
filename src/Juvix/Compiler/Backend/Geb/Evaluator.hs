module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
    module Juvix.Compiler.Backend.Geb.Evaluator.Data,
  )
where

import Control.DeepSeq
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Data
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromGebValue
import Juvix.Compiler.Backend.Geb.Translation.FromSource as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference as Geb

data RunEvalArgs = RunEvalArgs
  { -- | The input file
    _runEvalArgsInputFile :: Path Abs File,
    -- | The content of the input file
    _runEvalArgsContent :: Text,
    -- | The options
    _runEvalArgsEvaluatorOptions :: EvaluatorOptions
  }

makeLenses ''RunEvalArgs

runEval :: RunEvalArgs -> Either JuvixError GebValue
runEval RunEvalArgs {..} =
  case Geb.runParser _runEvalArgsInputFile _runEvalArgsContent of
    Right (ExpressionMorphism m) -> do
      let env :: Env =
            Env
              { _envEvaluatorOptions = _runEvalArgsEvaluatorOptions,
                _envContext = Context.empty
              }
      eval' env m
    Right _ -> Left (error @JuvixError objNoEvalMsg)
    Left err -> Left (JuvixError err)

objNoEvalMsg :: Text
objNoEvalMsg = "Geb objects cannot be evaluated, only morphisms."

eval ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r GebValue
eval = \case
  MorphismAbsurd _ -> error "Absurd can not be evaluated."
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
      _ -> error "First can only be applied to pairs."
  MorphismSecond s -> do
    res <- eval $ s ^. secondValue
    case res of
      GebValueMorphismPair pair ->
        return $ pair ^. valueMorphismPairRight
      _ -> error "Second can only be applied to pairs."
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
      _ -> error "Case can only be applied to sum types."

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
    _ -> error "Can only apply functions."

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
    _ -> error "Can only apply functions."

applyBinop ::
  Members '[Reader Env, Error JuvixError] r =>
  Binop ->
  Sem r GebValue
applyBinop binop = do
  left <- eval $ binop ^. binopLeft
  right <- eval $ binop ^. binopRight
  return $
    case (left, right) of
      (GebValueMorphismInteger l, GebValueMorphismInteger r) ->
        case binop ^. binopOpcode of
          OpAdd -> GebValueMorphismInteger $ l + r
          OpSub -> GebValueMorphismInteger $ l - r
          OpMul -> GebValueMorphismInteger $ l * r
          OpDiv -> GebValueMorphismInteger $ l `div` r
          OpMod -> GebValueMorphismInteger $ l `mod` r
          OpLt -> if l < r then valueTrue else valueFalse
          OpEq -> if l == r then valueTrue else valueFalse
      (m1, m2) -> case binop ^. binopOpcode of
        OpEq ->
          if
              | sameKind m1 m2 -> if m1 == m2 then valueTrue else valueFalse
              | otherwise -> error "Equality can only be applied to values of the same kind."
        _ ->
          error $
            "Canot apply operation:\n"
              <> (Geb.ppPrint (MorphismBinop binop))

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

eval' :: Env -> Morphism -> Either JuvixError GebValue
eval' env m = run . runError $ runReader env (eval m)

nf ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r Morphism
nf m = do
  -- First, we check the type (object) contained in the Morphism
  -- corresponds to the inferred type.
  ty <- runReader defaultInferenceEnv $ inferObject m
  -- Then, we evaluate the Morphism.
  val <- eval m
  -- Finally, we convert the result back to a Morphism.
  fromGebValue ty val

nf' :: Env -> Morphism -> Either JuvixError Morphism
nf' env m = run . runError $ runReader env (nf m)
