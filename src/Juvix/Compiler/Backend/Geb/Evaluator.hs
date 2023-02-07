module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
  )
where

import Control.DeepSeq
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference as Geb
import Juvix.Compiler.Core.Data.BinderList qualified as BinderList

type Context = BinderList.BinderList GebValue

emptyContext :: Context
emptyContext = BinderList.fromList []

consContext :: GebValue -> Context -> Context
consContext val env = BinderList.cons val env

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

data GebValue
  = GebValueMorphismBinop ValueMorphismBinop
  | GebValueMorphismCase ValueMorphismCase
  | GebValueMorphismInteger Integer
  | GebValueMorphismLeft GebValue
  | GebValueMorphismPair ValueMorphismPair
  | GebValueMorphismRight GebValue
  | GebValueMorphismUnit
  | GebValueClosure ValueClosure
  deriving stock (Generic)

instance NFData GebValue

newtype ValueMorphismLambda = ValueMorphismLambda
  { _valueMorphismLambdaFunction :: GebValue
  }
  deriving stock (Generic)

instance NFData ValueMorphismLambda

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }
  deriving stock (Generic)

instance NFData ValueMorphismPair

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }
  deriving stock (Generic)

instance NFData ValueMorphismCase

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }
  deriving stock (Generic)

instance NFData ValueMorphismBinop

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context,
    _valueClosureLambda :: Lambda
  }
  deriving stock (Generic)

instance NFData ValueClosure

instance HasAtomicity GebValue where
  atomicity = \case
    GebValueMorphismBinop {} -> Aggregate appFixity
    GebValueMorphismCase {} -> Aggregate appFixity
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
    _envContext :: Context
  }

makeLenses ''Env

defaultEnv :: Env
defaultEnv =
  Env
    { _envEvaluatorOptions = defaultEvaluatorOptions,
      _envContext = emptyContext
    }

eval ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r GebValue
eval = \case
  MorphismAbsurd _ -> error "Absurd can not be evaluated."
  MorphismVar var -> do
    ctx <- asks (^. envContext)
    return $ BinderList.lookup (var ^. varIndex) ctx
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
  MorphismApplication app -> do
    evalStrategy <- asks (^. envEvaluatorOptions . evaluatorOptionsEvalStrategy)
    let maybeForce :: GebValue -> GebValue
        maybeForce = case evalStrategy of
          CallByName -> id
          CallByValue -> force
    arg <- maybeForce <$> eval (app ^. applicationRight)
    fun <- eval $ app ^. applicationLeft
    case fun of
      GebValueClosure cls ->
        local (over envContext (consContext arg)) $
          eval (cls ^. valueClosureLambda . lambdaBody)
      _ -> error "Can only apply functions."
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
    vCaseLeft <- eval $ c ^. caseLeft
    vCaseRight <- eval $ c ^. caseRight
    return $
      GebValueMorphismCase
        ValueMorphismCase
          { _valueMorphismCaseOn = vCaseOn,
            _valueMorphismCaseLeft = vCaseLeft,
            _valueMorphismCaseRight = vCaseRight
          }

-- | Quoting a GebValue to a Morphism.
fromGebValue ::
  Members '[Reader Env, Error JuvixError] r =>
  Object ->
  GebValue ->
  Sem r Morphism
fromGebValue ty = \case
  GebValueMorphismInteger i -> case ty of
    ObjectInteger -> return $ MorphismInteger i
    _ -> error "fromGebValue: type mismatch. Expected Integer"
  GebValueMorphismUnit -> case ty of
    ObjectTerminal -> return MorphismUnit
    _ -> error "fromGebValue: type mismatch. Expected Unit"
  GebValueMorphismBinop m ->
    case ty of
      ObjectHom (Hom a bc) -> do
        left <- fromGebValue a (m ^. valueMorphismBinopLeft)
        right <- fromGebValue bc (m ^. valueMorphismBinopRight)
        return $
          MorphismBinop
            Binop
              { _binopOpcode = m ^. valueMorphismBinopOpcode,
                _binopLeft = left,
                _binopRight = right
              }
      _ -> error "fromGebValue: type mismatch (binop)"
  GebValueMorphismCase m -> case ty of
    ObjectHom h -> do
      let (a, b) = case h ^. homDomain of
            ObjectCoproduct coprod ->
              (coprod ^. coproductLeft, coprod ^. coproductRight)
            _ -> error "fromGebValue: type mismatch (case). Expected coproduct"
          c = h ^. homCodomain

          leftType :: Object
          leftType =
            ObjectHom $
              Hom
                { _homDomain = a,
                  _homCodomain = c
                }
          rightType :: Object
          rightType =
            ObjectHom $
              Hom
                { _homDomain = b,
                  _homCodomain = c
                }
          coprod' :: Object
          coprod' =
            ObjectCoproduct
              ( Coproduct
                  { _coproductLeft = a,
                    _coproductRight = b
                  }
              )
      cOn <- fromGebValue coprod' (m ^. valueMorphismCaseOn)
      cLeft <- fromGebValue leftType (m ^. valueMorphismCaseLeft)
      cRight <- fromGebValue rightType (m ^. valueMorphismCaseRight)
      return $
        MorphismCase
          Case
            { _caseOn = cOn,
              _caseLeft = cLeft,
              _caseRight = cRight,
              _caseLeftType = leftType,
              _caseRightType = rightType,
              _caseCodomainType = c
            }
    _ -> error "fromGebValue: type mismatch (case). Expected a homomorphism"
  GebValueMorphismLeft m -> case ty of
    ObjectCoproduct _ -> MorphismLeft <$> fromGebValue ty m
    _ -> error "fromGebValue: type mismatch (left). Expected a coproduct"
  GebValueMorphismRight m -> case ty of
    ObjectCoproduct _ -> MorphismRight <$> fromGebValue ty m
    _ -> error "fromGebValue: type mismatch (right). Expected a coproduct"
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
    _ -> error "fromGebValue: type mismatch (pair). Expected a product"
  GebValueClosure cls -> return $ MorphismLambda $ cls ^. valueClosureLambda

nf ::
  Members '[Reader Env, Error JuvixError] r =>
  Morphism ->
  Sem r Morphism
nf m = do
  ty <- inferObject m
  val <- eval m
  fromGebValue ty val

nf' ::
  Morphism ->
  Env ->
  Either JuvixError Morphism
nf' m env = run . runError $ runReader env (nf m)
