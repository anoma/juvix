module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
  )
where

import Control.DeepSeq
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference as Geb

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data GebValue
  = GebValueMorphismBinop ValueMorphismBinop
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft GebValue
  | GebValueMorphismPair ValueMorphismPair
  | GebValueMorphismRight GebValue
  | GebValueMorphismUnit
  | GebValueClosure ValueClosure
  deriving stock (Show, Generic)

instance NFData GebValue

newtype ValueMorphismLambda = ValueMorphismLambda
  { _valueMorphismLambdaFunction :: GebValue
  }
  deriving stock (Show, Generic)

instance NFData ValueMorphismLambda

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }
  deriving stock (Show, Generic)

instance NFData ValueMorphismPair

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }
  deriving stock (Show, Generic)

instance NFData ValueMorphismCase

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }
  deriving stock (Show, Generic)

instance NFData ValueMorphismBinop

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context GebValue,
    _valueClosureLambda :: Lambda
  }
  deriving stock (Show, Generic)

instance NFData ValueClosure

instance HasAtomicity GebValue where
  atomicity = \case
    GebValueMorphismBinop {} -> Aggregate appFixity
    GebValueMorphismInteger {} -> Atom
    GebValueMorphismLeft {} -> Aggregate appFixity
    GebValueMorphismPair {} -> Aggregate appFixity
    GebValueMorphismRight {} -> Aggregate appFixity
    GebValueMorphismUnit -> Atom
    GebValueClosure {} -> Aggregate appFixity

makeLenses ''ValueMorphismLambda
makeLenses ''ValueMorphismPair
makeLenses ''ValueMorphismCase
makeLenses ''ValueMorphismBinop
makeLenses ''ValueClosure

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------

data Env = Env
  { _envEvaluatorOptions :: EvaluatorOptions,
    _envContext :: Context GebValue
  }

makeLenses ''Env

defaultEvalEnv :: Env
defaultEvalEnv =
  Env
    { _envEvaluatorOptions = defaultEvaluatorOptions,
      _envContext = Context.empty
    }

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
  MorphismBinop op -> do
    left <- eval $ op ^. binopLeft
    right <- eval $ op ^. binopRight
    return $
      GebValueMorphismBinop $
        ValueMorphismBinop
          { _valueMorphismBinopOpcode = op ^. binopOpcode,
            _valueMorphismBinopLeft = left,
            _valueMorphismBinopRight = right
          }
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
  MorphismRight m ->
    GebValueMorphismRight <$> eval m
  MorphismCase c -> do
    vCaseOn <- eval $ c ^. caseOn
    -- Check
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

-- | Quoting a GebValue to a Morphism.
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
  GebValueMorphismBinop m -> do
    let lTerm = m ^. valueMorphismBinopLeft
        rTerm = m ^. valueMorphismBinopRight
    left <- fromGebValue ObjectInteger lTerm
    right <- fromGebValue ObjectInteger rTerm
    return $
      MorphismBinop
        Binop
          { _binopOpcode = m ^. valueMorphismBinopOpcode,
            _binopLeft = left,
            _binopRight = right
          }
  --   _ -> errorFromGebValue "type mismatch (binop)"
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

errorFromGebValue :: Text -> a
errorFromGebValue = error . ("fromGebValue: " <>)

eval' :: Env -> Morphism -> Either JuvixError GebValue
eval' env m = run . runError $ runReader env (eval m)

nf ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r Morphism
nf m = do
  ty <- runReader defaultInferenceEnv $ inferObject m
  val <- eval m
  fromGebValue ty val

nf' :: Env -> Morphism -> Either JuvixError Morphism
nf' env m = run . runError $ runReader env (nf m)
