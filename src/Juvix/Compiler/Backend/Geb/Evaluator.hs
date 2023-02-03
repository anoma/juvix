module Juvix.Compiler.Backend.Geb.Evaluator
  ( module Juvix.Compiler.Backend.Geb.Evaluator,
    module Juvix.Compiler.Backend.Geb.Evaluator.Options,
  )
where

import Data.List.NonEmpty
import Juvix.Compiler.Backend.Geb.Evaluator.Options
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Prelude

eval :: EvaluatorOptions -> Geb.Morphism -> Geb.Morphism
eval opts = last . steps opts

steps :: EvaluatorOptions -> Geb.Morphism -> NonEmpty Geb.Morphism
steps opts e
  | e == s = e :| []
  | otherwise = e <| steps opts s
  where
    strategy :: EvalStrategy
    strategy = opts ^. evaluatorOptionsEvalStrategy
    s = case strategy of
      CallByValue -> evalCallByValue e
      CallByName -> evalCallByName e
      Full -> evalFull e

-- | Is the given Geb expression in normal form?
isGebValue :: Geb.Morphism -> Bool
isGebValue = \case
  Geb.MorphismAbsurd m -> isGebValue m
  Geb.MorphismUnit -> True
  Geb.MorphismLeft m -> isGebValue m
  Geb.MorphismRight m -> isGebValue m
  Geb.MorphismCase (Geb.Case {..}) -> case _caseOn of
    Geb.MorphismLeft _ -> False
    Geb.MorphismRight _ -> False
    _ -> all isGebValue [_caseOn, _caseLeft, _caseRight]
  Geb.MorphismPair (Geb.Pair {..}) ->
    isGebValue _pairLeft && isGebValue _pairRight
  Geb.MorphismFirst (Geb.First {..}) ->
    case _firstValue of
      Geb.MorphismPair _ -> False
      _ -> isGebValue _firstValue
  Geb.MorphismSecond (Geb.Second {..}) ->
    case _secondValue of
      Geb.MorphismPair _ -> False
      _ -> isGebValue _secondValue
  Geb.MorphismLambda _ -> True
  Geb.MorphismApplication (Geb.Application {..}) ->
    case _applicationLeft of
      Geb.MorphismLambda _ -> False
      (Geb.MorphismVar _) -> isGebValue _applicationRight
      _ -> isGebValue _applicationLeft && isGebValue _applicationRight
  Geb.MorphismVar _ -> True
  Geb.MorphismInteger _ -> True
  Geb.MorphismBinop (Geb.Binop {..}) ->
    isGebValue _binopLeft && isGebValue _binopRight

-- | Apply beta reduction to a term
applyBeta :: Geb.Morphism -> Geb.Morphism
applyBeta = \case
  m@(Geb.MorphismApplication (Geb.Application {..})) -> do
    case _applicationLeft of
      Geb.MorphismLambda (Geb.Lambda {..}) ->
        substitute 0 _applicationRight _lambdaBody
      _ -> m
  e -> e

type Variable = Geb.Morphism

type DeBruijnTarget = Int

substitute :: DeBruijnTarget -> Variable -> Geb.Morphism -> Geb.Morphism
substitute idx x = \case
  v@(Geb.MorphismVar (Geb.Var {..})) ->
    if
        | _varIndex == idx -> x -- Woula, we found it!
        | _varIndex > idx -> -- TODO: double-check this
            Geb.MorphismVar (Geb.Var {Geb._varIndex = _varIndex - 1})
        | otherwise -> v
  Geb.MorphismAbsurd m -> Geb.MorphismAbsurd $ substitute idx x m
  Geb.MorphismUnit -> Geb.MorphismUnit
  Geb.MorphismLeft m -> Geb.MorphismLeft $ substitute idx x m
  Geb.MorphismRight m -> Geb.MorphismRight $ substitute idx x m
  Geb.MorphismCase (Geb.Case {..}) ->
    Geb.MorphismCase
      Geb.Case
        { _caseOn = substitute idx x _caseOn,
          _caseLeft = substitute idx x _caseLeft,
          _caseRight = substitute idx x _caseRight,
          ..
        }
  Geb.MorphismPair (Geb.Pair {..}) ->
    Geb.MorphismPair
      Geb.Pair
        { _pairLeft = substitute idx x _pairLeft,
          _pairRight = substitute idx x _pairRight,
          ..
        }
  Geb.MorphismFirst (Geb.First {..}) ->
    Geb.MorphismFirst $
      Geb.First {_firstValue = substitute idx x _firstValue, ..}
  Geb.MorphismSecond (Geb.Second {..}) ->
    Geb.MorphismSecond $
      Geb.Second {_secondValue = substitute idx x _secondValue, ..}
  -- We have to increment the index of the variable we are substituting for, as
  -- we are going deeper into the lambda, but at the same time, the body of
  -- the lambda is one level deeper than the lambda itself, so, we also have to
  -- increment all the inner variables by 1.
  Geb.MorphismLambda (Geb.Lambda {..}) ->
    Geb.MorphismLambda $
      Geb.Lambda
        { _lambdaBody =
            substitute
              (idx + 1)
              x
              (increment 0 _lambdaBody),
          ..
        }
  Geb.MorphismApplication (Geb.Application {..}) ->
    Geb.MorphismApplication $
      Geb.Application
        { _applicationLeft = substitute idx x _applicationLeft,
          _applicationRight = substitute idx x _applicationRight,
          ..
        }
  t@(Geb.MorphismInteger _) -> t
  (Geb.MorphismBinop (Geb.Binop {..})) ->
    Geb.MorphismBinop $
      Geb.Binop
        { _binopLeft = substitute idx x _binopLeft,
          _binopRight = substitute idx x _binopRight,
          ..
        }

increment :: Int -> Geb.Morphism -> Geb.Morphism
increment n = \case
  (Geb.MorphismLambda (Geb.Lambda {..})) ->
    Geb.MorphismLambda $
      Geb.Lambda
        { _lambdaBody = increment (n + 1) _lambdaBody,
          ..
        }
  Geb.MorphismApplication (Geb.Application {..}) ->
    Geb.MorphismApplication $
      Geb.Application
        { _applicationLeft = increment n _applicationLeft,
          _applicationRight = increment n _applicationRight,
          ..
        }
  Geb.MorphismAbsurd e -> Geb.MorphismAbsurd $ increment n e
  Geb.MorphismUnit -> Geb.MorphismUnit
  Geb.MorphismLeft m -> Geb.MorphismLeft $ increment n m
  Geb.MorphismRight m -> Geb.MorphismRight $ increment n m
  Geb.MorphismCase (Geb.Case {..}) ->
    Geb.MorphismCase
      Geb.Case
        { _caseOn = increment n _caseOn,
          _caseLeft = increment n _caseLeft,
          _caseRight = increment n _caseRight,
          ..
        }
  Geb.MorphismPair (Geb.Pair {..}) ->
    Geb.MorphismPair
      Geb.Pair
        { _pairLeft = increment n _pairLeft,
          _pairRight = increment n _pairRight,
          ..
        }
  Geb.MorphismFirst (Geb.First {..}) ->
    Geb.MorphismFirst $
      Geb.First {_firstValue = increment n _firstValue, ..}
  Geb.MorphismSecond (Geb.Second {..}) ->
    Geb.MorphismSecond $
      Geb.Second {_secondValue = increment n _secondValue, ..}
  t@(Geb.MorphismVar (Geb.Var {..})) ->
    if
        | _varIndex > n -> Geb.MorphismVar $ Geb.Var {_varIndex = _varIndex + 1}
        | otherwise -> t
  t@(Geb.MorphismInteger _) -> t
  (Geb.MorphismBinop (Geb.Binop {..})) ->
    Geb.MorphismBinop $
      Geb.Binop
        { _binopLeft = increment n _binopLeft,
          _binopRight = increment n _binopRight,
          ..
        }

evalCallByValue :: Geb.Morphism -> Geb.Morphism
evalCallByValue = \case
  Geb.MorphismAbsurd m -> Geb.MorphismAbsurd $ evalCallByValue m
  Geb.MorphismUnit -> Geb.MorphismUnit
  Geb.MorphismLeft m -> Geb.MorphismLeft $ evalCallByValue m
  Geb.MorphismRight m -> Geb.MorphismRight $ evalCallByValue m
  Geb.MorphismCase (Geb.Case {..}) -> case _caseOn of
    Geb.MorphismLeft a ->
      Geb.MorphismApplication
        Geb.Application
          { _applicationLeft = _caseLeft,
            _applicationRight = a,
            _applicationDomainType = _caseLeftType,
            _applicationCodomainType = _caseCodomainType
          }
    Geb.MorphismRight a ->
      Geb.MorphismApplication
        Geb.Application
          { _applicationLeft = _caseRight,
            _applicationRight = a,
            _applicationDomainType = _caseRightType,
            _applicationCodomainType = _caseCodomainType
          }
    _ ->
      Geb.MorphismCase
        Geb.Case
          { _caseOn = evalCallByValue _caseOn,
            _caseLeft = evalCallByValue _caseLeft,
            _caseRight = evalCallByValue _caseRight,
            ..
          }
  Geb.MorphismPair (Geb.Pair {..}) ->
    Geb.MorphismPair
      Geb.Pair
        { _pairLeft = evalCallByValue _pairLeft,
          _pairRight = evalCallByValue _pairRight,
          ..
        }
  Geb.MorphismFirst (Geb.First {..}) ->
    case _firstValue of
      (Geb.MorphismPair (Geb.Pair {..})) -> _pairLeft
      m ->
        Geb.MorphismFirst $
          Geb.First {_firstValue = evalCallByValue m, ..}
  Geb.MorphismSecond (Geb.Second {..}) ->
    case _secondValue of
      (Geb.MorphismPair (Geb.Pair {..})) -> _pairRight
      m ->
        Geb.MorphismSecond $
          Geb.Second {_secondValue = evalCallByValue m, ..}
  l@(Geb.MorphismLambda _) -> l
  m@(Geb.MorphismApplication (Geb.Application {..})) ->
    case _applicationLeft of
      -- In precense of a lambda, we can apply beta reduction
      -- as long as the right hand side is a value
      Geb.MorphismLambda _ -> do
        if
            | isGebValue _applicationRight -> applyBeta m
            | otherwise ->
                Geb.MorphismApplication $
                  Geb.Application
                    { Geb._applicationRight = evalCallByValue _applicationRight,
                      ..
                    }
      (Geb.MorphismVar _) ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationRight = evalCallByValue _applicationRight,
              ..
            }
      -- (App (App f g) x) = App (eval (App f g)) (eval x)
      n@(Geb.MorphismApplication _) ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationLeft = evalCallByValue n,
              _applicationRight = evalCallByValue _applicationRight,
              ..
            }
      -- (App a b) = App (eval a) (eval b)
      _ ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationLeft = evalCallByValue _applicationLeft,
              _applicationRight = evalCallByValue _applicationRight,
              ..
            }
  v@(Geb.MorphismVar _) -> v
  i@(Geb.MorphismInteger _) -> i
  (Geb.MorphismBinop (Geb.Binop {..})) ->
    Geb.MorphismBinop $
      Geb.Binop
        { _binopLeft = evalCallByValue _binopLeft,
          _binopRight = evalCallByValue _binopRight,
          ..
        }

evalCallByName :: Geb.Morphism -> Geb.Morphism
evalCallByName = \case
  Geb.MorphismAbsurd m -> Geb.MorphismAbsurd $ evalCallByName m
  Geb.MorphismUnit -> Geb.MorphismUnit
  Geb.MorphismLeft m -> Geb.MorphismLeft $ evalCallByName m
  Geb.MorphismRight m -> Geb.MorphismRight $ evalCallByName m
  Geb.MorphismCase (Geb.Case {..}) -> case _caseOn of
    Geb.MorphismLeft a ->
      Geb.MorphismApplication
        Geb.Application
          { _applicationLeft = _caseLeft,
            _applicationRight = a,
            _applicationDomainType = _caseLeftType,
            _applicationCodomainType = _caseCodomainType
          }
    Geb.MorphismRight b ->
      Geb.MorphismApplication
        Geb.Application
          { _applicationLeft = _caseRight,
            _applicationRight = b,
            _applicationDomainType = _caseRightType,
            _applicationCodomainType = _caseCodomainType
          }
    _ ->
      Geb.MorphismCase
        Geb.Case
          { _caseOn = evalCallByName _caseOn,
            _caseLeft = evalCallByName _caseLeft,
            _caseRight = evalCallByName _caseRight,
            ..
          }
  Geb.MorphismPair (Geb.Pair {..}) ->
    Geb.MorphismPair
      Geb.Pair
        { _pairLeft = evalCallByName _pairLeft,
          _pairRight = evalCallByName _pairRight,
          ..
        }
  Geb.MorphismFirst (Geb.First {..}) ->
    case _firstValue of
      (Geb.MorphismPair (Geb.Pair {..})) -> _pairLeft
      m ->
        Geb.MorphismFirst $
          Geb.First {_firstValue = evalCallByName m, ..}
  Geb.MorphismSecond (Geb.Second {..}) ->
    case _secondValue of
      (Geb.MorphismPair (Geb.Pair {..})) -> _pairLeft
      m ->
        Geb.MorphismSecond $
          Geb.Second {_secondValue = evalCallByName m, ..}
  Geb.MorphismLambda (Geb.Lambda {..}) ->
    Geb.MorphismLambda $
      Geb.Lambda
        { _lambdaBody = evalCallByName _lambdaBody,
          ..
        }
  m@(Geb.MorphismApplication (Geb.Application {..})) ->
    case _applicationLeft of
      Geb.MorphismLambda _ -> applyBeta m
      (Geb.MorphismVar _) ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationRight = evalCallByName _applicationRight,
              ..
            }
      n@(Geb.MorphismApplication _) ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationLeft = evalCallByName n,
              _applicationRight = evalCallByName _applicationRight,
              ..
            }
      _ ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationLeft = evalCallByName _applicationLeft,
              _applicationRight = evalCallByName _applicationRight,
              ..
            }
  v@(Geb.MorphismVar _) -> v
  i@(Geb.MorphismInteger _) -> i
  (Geb.MorphismBinop (Geb.Binop {..})) ->
    Geb.MorphismBinop $
      Geb.Binop
        { _binopLeft = evalCallByName _binopLeft,
          _binopRight = evalCallByName _binopRight,
          ..
        }

evalFull :: Geb.Morphism -> Geb.Morphism
evalFull = \case
  Geb.MorphismAbsurd m -> Geb.MorphismAbsurd $ evalFull m
  Geb.MorphismUnit -> Geb.MorphismUnit
  Geb.MorphismLeft m -> Geb.MorphismLeft $ evalFull m
  Geb.MorphismRight m -> Geb.MorphismRight $ evalFull m
  Geb.MorphismCase (Geb.Case {..}) -> case _caseOn of
    Geb.MorphismLeft a ->
      Geb.MorphismApplication
        Geb.Application
          { _applicationLeft = _caseLeft,
            _applicationRight = a,
            _applicationDomainType = _caseLeftType,
            _applicationCodomainType = _caseCodomainType
          }
    Geb.MorphismRight b ->
      Geb.MorphismApplication
        Geb.Application
          { _applicationLeft = _caseRight,
            _applicationRight = b,
            _applicationDomainType = _caseRightType,
            _applicationCodomainType = _caseCodomainType
          }
    _ ->
      Geb.MorphismCase
        Geb.Case
          { _caseOn = evalFull _caseOn,
            _caseLeft = evalFull _caseLeft,
            _caseRight = evalFull _caseRight,
            ..
          }
  Geb.MorphismPair (Geb.Pair {..}) ->
    Geb.MorphismPair
      Geb.Pair
        { _pairLeft = evalFull _pairLeft,
          _pairRight = evalFull _pairRight,
          ..
        }
  Geb.MorphismFirst (Geb.First {..}) ->
    case _firstValue of
      (Geb.MorphismPair (Geb.Pair {..})) -> _pairLeft
      m ->
        Geb.MorphismFirst $
          Geb.First {_firstValue = evalFull m, ..}
  Geb.MorphismSecond (Geb.Second {..}) ->
    case _secondValue of
      (Geb.MorphismPair (Geb.Pair {..})) -> _pairLeft
      m ->
        Geb.MorphismSecond $
          Geb.Second {_secondValue = evalFull m, ..}
  Geb.MorphismLambda (Geb.Lambda {..}) ->
    Geb.MorphismLambda $
      Geb.Lambda
        { _lambdaBody = evalFull _lambdaBody,
          ..
        }
  m@(Geb.MorphismApplication (Geb.Application {..})) ->
    case _applicationLeft of
      Geb.MorphismLambda _ -> applyBeta m
      (Geb.MorphismVar _) ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationRight = evalFull _applicationRight,
              ..
            }
      n@(Geb.MorphismApplication _) ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationLeft = evalFull n,
              _applicationRight = evalFull _applicationRight,
              ..
            }
      _ ->
        Geb.MorphismApplication $
          Geb.Application
            { _applicationLeft = evalFull _applicationLeft,
              _applicationRight = evalFull _applicationRight,
              ..
            }
  v@(Geb.MorphismVar _) -> v
  i@(Geb.MorphismInteger _) -> i
  (Geb.MorphismBinop (Geb.Binop {..})) ->
    Geb.MorphismBinop $
      Geb.Binop
        { _binopLeft = evalFull _binopLeft,
          _binopRight = evalFull _binopRight,
          ..
        }
