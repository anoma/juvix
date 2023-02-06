module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
  )
where

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
  | GebValueMorphismLambda ValueMorphismLambda
  | GebValueMorphismLeft GebValue
  | GebValueMorphismPair ValueMorphismPair
  | GebValueMorphismRight GebValue
  | GebValueMorphismUnit

newtype ValueMorphismLambda = ValueMorphismLambda
  { _valueMorphismLambdaFunction :: GebValue -> GebValue
  }

data ValueMorphismPair = ValueMorphismPair
  { _valueMorphismPairLeft :: GebValue,
    _valueMorphismPairRight :: GebValue
  }

data ValueMorphismCase = ValueMorphismCase
  { _valueMorphismCaseOn :: GebValue,
    _valueMorphismCaseLeft :: GebValue,
    _valueMorphismCaseRight :: GebValue
  }

data ValueMorphismBinop = ValueMorphismBinop
  { _valueMorphismBinopOpcode :: Geb.Opcode,
    _valueMorphismBinopLeft :: GebValue,
    _valueMorphismBinopRight :: GebValue
  }

instance HasAtomicity GebValue where
  atomicity = \case
    GebValueMorphismBinop {} -> Aggregate appFixity
    GebValueMorphismCase {} -> Aggregate appFixity
    GebValueMorphismInteger {} -> Atom
    GebValueMorphismLambda {} -> Aggregate appFixity
    GebValueMorphismLeft {} -> Aggregate appFixity
    GebValueMorphismPair {} -> Aggregate appFixity
    GebValueMorphismRight {} -> Aggregate appFixity
    GebValueMorphismUnit -> Atom

instance HasAtomicity (Either JuvixError GebValue) where
  atomicity = \case
    Left _ -> Atom
    Right val -> atomicity val

makeLenses ''ValueMorphismLambda
makeLenses ''ValueMorphismPair
makeLenses ''ValueMorphismCase
makeLenses ''ValueMorphismBinop

--------------------------------------------------------------------------------
-- Evaluator
--------------------------------------------------------------------------------

data EvalArgs = EvalArgs
  { _evalOptions :: EvaluatorOptions,
    _evalContext :: Context,
    _evalTerm :: Geb.Morphism
  }

makeLenses ''EvalArgs

eval' :: EvalArgs -> Either JuvixError Geb.Morphism
eval' opts = do
  let val' = eval opts
      obj' = Geb.inferObject (opts ^. evalTerm)
  case (val', obj') of
    (Right val, Right obj) -> fromGebValue obj val
    (Left err, _) -> Left err
    (_, Left err) -> Left err

eval :: EvalArgs -> Either JuvixError GebValue
eval EvalArgs {..} = case _evalTerm of
  Geb.MorphismAbsurd _ -> Left $ error "Absurd can not be evaluated."
  Geb.MorphismVar var -> Right $ BinderList.lookup (var ^. Geb.varIndex) _evalContext
  Geb.MorphismUnit -> Right GebValueMorphismUnit
  Geb.MorphismInteger i -> Right $ GebValueMorphismInteger i
  Geb.MorphismPair pair -> do
    let left = eval EvalArgs {_evalTerm = pair ^. Geb.pairLeft, ..}
        right = eval EvalArgs {_evalTerm = pair ^. Geb.pairRight, ..}
    case (left, right) of
      (Right l, Right r) ->
        Right $
          GebValueMorphismPair $
            ValueMorphismPair
              { _valueMorphismPairLeft = l,
                _valueMorphismPairRight = r
              }
      (Left err, _) -> Left err
      (_, Left err) -> Left err
  Geb.MorphismFirst f -> do
    let res = eval EvalArgs {_evalTerm = f ^. Geb.firstValue, ..}
    case res of
      Right (GebValueMorphismPair pair) ->
        Right (pair ^. valueMorphismPairLeft)
      Right _ -> Left $ error "First can only be applied to pairs."
      Left err -> Left err
  Geb.MorphismSecond s -> do
    case eval (EvalArgs {_evalTerm = s ^. Geb.secondValue, ..}) of
      Right (GebValueMorphismPair pair) ->
        Right (pair ^. valueMorphismPairRight)
      Right _ -> Left $ error "Second can only be applied to pairs."
      Left err -> Left err
  Geb.MorphismBinop op -> do
    let left = eval EvalArgs {_evalTerm = op ^. Geb.binopLeft, ..}
        right = eval EvalArgs {_evalTerm = op ^. Geb.binopRight, ..}
    case (left, right) of
      (Right l, Right r) ->
        Right . GebValueMorphismBinop $
          ValueMorphismBinop
            { _valueMorphismBinopOpcode = op ^. Geb.binopOpcode,
              _valueMorphismBinopLeft = l,
              _valueMorphismBinopRight = r
            }
      (Left err, _) -> Left err
      (_, Left err) -> Left err
  Geb.MorphismApplication app -> do
    let fun = eval EvalArgs {_evalTerm = app ^. Geb.applicationLeft, ..}
        arg = eval EvalArgs {_evalTerm = app ^. Geb.applicationRight, ..}
    case _evalOptions ^. evaluatorOptionsEvalStrategy of
      CallByName -> case fun of
        Right (GebValueMorphismLambda lambda) ->
          (lambda ^. valueMorphismLambdaFunction) <$> arg
        Right _ -> Left $ error "Can only apply functions."
        Left err -> Left err
      CallByValue -> case arg of
        (Right x) -> case fun of
          Right (GebValueMorphismLambda lambda) ->
            Right $ (lambda ^. valueMorphismLambdaFunction) x
          Right _ -> Left $ error "Can only apply functions."
          Left err -> Left err
        (Left err) -> Left err
  Geb.MorphismLambda lambda -> do
    Right $
      GebValueMorphismLambda $
        ValueMorphismLambda
          { _valueMorphismLambdaFunction = \x ->
              fromRight (error "Unexpected error evaluating a lambda") $
                eval
                  EvalArgs
                    { _evalTerm = lambda ^. Geb.lambdaBody,
                      _evalContext = BinderList.cons x _evalContext,
                      ..
                    }
          }
  Geb.MorphismLeft m ->
    GebValueMorphismLeft <$> eval EvalArgs {_evalTerm = m, ..}
  Geb.MorphismRight m ->
    GebValueMorphismRight <$> eval EvalArgs {_evalTerm = m, ..}
  Geb.MorphismCase c -> do
    let vCaseOn' = eval EvalArgs {_evalTerm = c ^. Geb.caseOn, ..}
        vCaseLeft' = eval EvalArgs {_evalTerm = c ^. Geb.caseLeft, ..}
        vCaseRight' = eval EvalArgs {_evalTerm = c ^. Geb.caseRight, ..}
    case (vCaseOn', vCaseLeft', vCaseRight') of
      (Right vCaseOn, Right vCaseLeft, Right vCaseRight) ->
        Right $
          GebValueMorphismCase $
            ValueMorphismCase
              { _valueMorphismCaseOn = vCaseOn,
                _valueMorphismCaseLeft = vCaseLeft,
                _valueMorphismCaseRight = vCaseRight
              }
      (Left err, _, _) -> Left err
      (_, Left err, _) -> Left err
      (_, _, Left err) -> Left err

-- | Quoting a GebValue to a Geb.Morphism.
fromGebValue :: Geb.Object -> GebValue -> Either JuvixError Geb.Morphism
fromGebValue ty = \case
  GebValueMorphismInteger i -> case ty of
    Geb.ObjectInteger -> Right $ Geb.MorphismInteger i
    _ -> Left $ error "fromGebValue: type mismatch. Expected Integer"
  GebValueMorphismUnit -> case ty of
    Geb.ObjectTerminal -> Right Geb.MorphismUnit
    _ -> Left $ error "fromGebValue: type mismatch. Expected Unit"
  GebValueMorphismBinop m ->
    case ty of
      Geb.ObjectHom (Geb.Hom a bc) -> do
        let left' = fromGebValue a (m ^. valueMorphismBinopLeft)
            right' = fromGebValue bc (m ^. valueMorphismBinopRight)
        case (left', right') of
          (Right left, Right right) ->
            Right $
              Geb.MorphismBinop $
                Geb.Binop
                  { _binopOpcode = m ^. valueMorphismBinopOpcode,
                    _binopLeft = left,
                    _binopRight = right
                  }
          (Left err, _) -> Left err
          (_, Left err) -> Left err
      _ -> Left $ error "fromGebValue: type mismatch (binop)"
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
          caseOn' = fromGebValue coprod' (m ^. valueMorphismCaseOn)
          caseLeft' = fromGebValue leftType (m ^. valueMorphismCaseLeft)
          caseRight' = fromGebValue rightType (m ^. valueMorphismCaseRight)

      case (caseOn', caseLeft', caseRight') of
        (Right caseOn, Right caseLeft, Right caseRight) ->
          Right $
            Geb.MorphismCase $
              Geb.Case
                { _caseOn = caseOn,
                  _caseLeft = caseLeft,
                  _caseRight = caseRight,
                  _caseLeftType = leftType,
                  _caseRightType = rightType,
                  _caseCodomainType = c
                }
        (Left err, _, _) -> Left err
        (_, Left err, _) -> Left err
        (_, _, Left err) -> Left err
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
          pairLeft' = fromGebValue a (m ^. valueMorphismPairLeft)
          pairRight' = fromGebValue b (m ^. valueMorphismPairRight)
      case (pairLeft', pairRight') of
        (Right pLeft, Right pRight) ->
          Right $
            Geb.MorphismPair $
              Geb.Pair
                { _pairLeft = pLeft,
                  _pairRight = pRight,
                  _pairLeftType = a,
                  _pairRightType = b
                }
        (Left err, _) -> Left err
        (_, Left err) -> Left err
    _ -> Left $ error "fromGebValue: type mismatch (pair). Expected a product"
  GebValueMorphismLambda _ -> case ty of
    Geb.ObjectHom _ -> Left $ error "fromGebValue: Lambda can not be converted to Geb morphisms."
    _ -> error "fromGebValue: type mismatch (lambda). Expected a homomorphism"
