module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
  )
where

import Control.DeepSeq
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference as Geb
import Juvix.Compiler.Core.Data.BinderList qualified as BinderList
import Juvix.Prelude

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

instance NFData ValueMorphismCase

instance NFData ValueMorphismPair

instance NFData ValueMorphismLambda

instance NFData ValueMorphismBinop

instance NFData ValueClosure

newtype ValueMorphismLambda = ValueMorphismLambda
  { _valueMorphismLambdaFunction :: GebValue
  }
  deriving stock (Generic)

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }
  deriving stock (Generic)

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }
  deriving stock (Generic)

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Geb.Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }
  deriving stock (Generic)

data ValueClosure = ValueClosure
  { _valueClosureEnv :: Context,
    _valueClosureLambda :: Geb.Lambda
  }
  deriving stock (Generic)

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
  { _envOptions :: EvaluatorOptions,
    _envContext :: Context
  }

makeLenses ''Env

-- eval' ::
--   Members '[Reader Env, Error JuvixError] r =>
--   Geb.Morphism ->
--   Sem r Geb.Morphism
-- eval' m = do
--   val <- eval m
--   obj <- Geb.inferObject m
--   fromGebValue obj val

eval ::
  Members '[Reader Env, Error JuvixError] r =>
  Geb.Morphism ->
  Sem r GebValue
eval = \case
  Geb.MorphismAbsurd _ -> error "Absurd can not be evaluated."
  Geb.MorphismVar var -> do
    ctx <- asks (^. envContext)
    return $ BinderList.lookup (var ^. Geb.varIndex) ctx
  Geb.MorphismUnit -> return GebValueMorphismUnit
  Geb.MorphismInteger i -> return $ GebValueMorphismInteger i
  Geb.MorphismPair pair -> do
    left <- eval $ pair ^. Geb.pairLeft
    right <- eval $ pair ^. Geb.pairRight
    return $
      GebValueMorphismPair $
        ValueMorphismPair
          { _valueMorphismPairLeft = left,
            _valueMorphismPairRight = right
          }
  Geb.MorphismFirst f -> do
    res <- eval $ f ^. Geb.firstValue
    case res of
      GebValueMorphismPair pair ->
        return $ pair ^. valueMorphismPairLeft
      _ -> error "First can only be applied to pairs."
  Geb.MorphismSecond s -> do
    res <- eval $ s ^. Geb.secondValue
    case res of
      GebValueMorphismPair pair ->
        return $ pair ^. valueMorphismPairRight
      _ -> error "Second can only be applied to pairs."
  Geb.MorphismBinop op -> do
    left <- eval $ op ^. Geb.binopLeft
    right <- eval $ op ^. Geb.binopRight
    return $
      GebValueMorphismBinop $
        ValueMorphismBinop
          { _valueMorphismBinopOpcode = op ^. Geb.binopOpcode,
            _valueMorphismBinopLeft = left,
            _valueMorphismBinopRight = right
          }
  Geb.MorphismApplication app -> do
    evalStrategy <- asks (^. envOptions . evaluatorOptionsEvalStrategy)
    let maybeForce :: GebValue -> GebValue
        maybeForce = case evalStrategy of
          CallByName -> id
          CallByValue -> force
    arg <- maybeForce <$> eval (app ^. Geb.applicationRight)
    fun <- eval $ app ^. Geb.applicationLeft
    case fun of
      GebValueClosure cls ->
        local (over envContext (consContext arg)) $
          eval (cls ^. valueClosureLambda . Geb.lambdaBody)
      _ -> error "Can only apply functions."
  Geb.MorphismLambda lambda -> do
    ctx <- asks (^. envContext)
    return $
      GebValueClosure $
        ValueClosure
          { _valueClosureLambda = lambda,
            _valueClosureEnv = ctx
          }
  Geb.MorphismLeft m -> GebValueMorphismLeft <$> eval m
  Geb.MorphismRight m ->
    GebValueMorphismRight <$> eval m
  Geb.MorphismCase c -> do
    vCaseOn <- eval $ c ^. Geb.caseOn
    vCaseLeft <- eval $ c ^. Geb.caseLeft
    vCaseRight <- eval $ c ^. Geb.caseRight
    return $
      GebValueMorphismCase
        ValueMorphismCase
          { _valueMorphismCaseOn = vCaseOn,
            _valueMorphismCaseLeft = vCaseLeft,
            _valueMorphismCaseRight = vCaseRight
          }

-- -- | Quoting a GebValue to a Geb.Morphism.
fromGebValue ::
  Members '[Reader Env, Error JuvixError] r =>
  Geb.Object ->
  GebValue ->
  Sem r Geb.Morphism
fromGebValue ty = \case
  GebValueMorphismInteger i -> case ty of
    Geb.ObjectInteger -> return $ Geb.MorphismInteger i
    _ -> error "fromGebValue: type mismatch. Expected Integer"
  GebValueMorphismUnit -> case ty of
    Geb.ObjectTerminal -> return Geb.MorphismUnit
    _ -> error "fromGebValue: type mismatch. Expected Unit"
  GebValueMorphismBinop m ->
    case ty of
      Geb.ObjectHom (Geb.Hom a bc) -> do
        left <- fromGebValue a (m ^. valueMorphismBinopLeft)
        right <- fromGebValue bc (m ^. valueMorphismBinopRight)
        return $
          Geb.MorphismBinop
            Geb.Binop
              { _binopOpcode = m ^. valueMorphismBinopOpcode,
                _binopLeft = left,
                _binopRight = right
              }
      _ -> error "fromGebValue: type mismatch (binop)"
  GebValueMorphismCase m -> case ty of
    Geb.ObjectHom h -> do
      let (a, b) = case h ^. Geb.homDomain of
            Geb.ObjectCoproduct coprod ->
              (coprod ^. Geb.coproductLeft, coprod ^. Geb.coproductRight)
            _ -> error "fromGebValue: type mismatch (case). Expected coproduct"
          c = h ^. Geb.homCodomain

          leftType :: Geb.Object
          leftType =
            Geb.ObjectHom $
              Geb.Hom
                { _homDomain = a,
                  _homCodomain = c
                }
          rightType :: Geb.Object
          rightType =
            Geb.ObjectHom $
              Geb.Hom
                { _homDomain = b,
                  _homCodomain = c
                }
          coprod' :: Geb.Object
          coprod' =
            Geb.ObjectCoproduct
              ( Geb.Coproduct
                  { _coproductLeft = a,
                    _coproductRight = b
                  }
              )
      caseOn <- fromGebValue coprod' (m ^. valueMorphismCaseOn)
      caseLeft <- fromGebValue leftType (m ^. valueMorphismCaseLeft)
      caseRight <- fromGebValue rightType (m ^. valueMorphismCaseRight)
      return $
        Geb.MorphismCase
          Geb.Case
            { _caseOn = caseOn,
              _caseLeft = caseLeft,
              _caseRight = caseRight,
              _caseLeftType = leftType,
              _caseRightType = rightType,
              _caseCodomainType = c
            }
    _ -> error "fromGebValue: type mismatch (case). Expected a homomorphism"
  GebValueMorphismLeft m -> case ty of
    Geb.ObjectCoproduct _ -> Geb.MorphismLeft <$> fromGebValue ty m
    _ -> error "fromGebValue: type mismatch (left). Expected a coproduct"
  GebValueMorphismRight m -> case ty of
    Geb.ObjectCoproduct _ -> Geb.MorphismRight <$> fromGebValue ty m
    _ -> error "fromGebValue: type mismatch (right). Expected a coproduct"
  GebValueMorphismPair m -> case ty of
    Geb.ObjectProduct prod -> do
      let (a, b) = (prod ^. Geb.productLeft, prod ^. Geb.productRight)
      pLeft <- fromGebValue a (m ^. valueMorphismPairLeft)
      pRight <- fromGebValue b (m ^. valueMorphismPairRight)
      return $
        Geb.MorphismPair
          Geb.Pair
            { _pairLeft = pLeft,
              _pairRight = pRight,
              _pairLeftType = a,
              _pairRightType = b
            }
    _ -> error "fromGebValue: type mismatch (pair). Expected a product"
  GebValueClosure cls -> case ty of
    _ -> error "TODO"
  -- Geb.ObjectHom _ -> Left $ error "fromGebValue: Lambda can not be converted to Geb morphisms."
  _ -> error "fromGebValue: type mismatch (lambda). Expected a homomorphism"
