module Juvix.Compiler.Backend.Geb.Extra where

import Juvix.Compiler.Backend.Geb.Language

-- | Destructs a product in a right-associative way, e.g., (a, (b, c)) is
-- destructed to [a, b, c]
destructProduct :: Object -> [Object]
destructProduct = \case
  ObjectProduct Product {..} -> _productLeft : destructProduct _productRight
  x -> [x]

objectBool :: Object
objectBool = ObjectCoproduct (Coproduct ObjectTerminal ObjectTerminal)

morphismTrue :: Morphism
morphismTrue =
  MorphismLeft
    LeftInj
      { _leftInjLeftType = ObjectTerminal,
        _leftInjRightType = ObjectTerminal,
        _leftInjValue = MorphismUnit
      }

morphismFalse :: Morphism
morphismFalse =
  MorphismRight
    RightInj
      { _rightInjLeftType = ObjectTerminal,
        _rightInjRightType = ObjectTerminal,
        _rightInjValue = MorphismUnit
      }

-- | NOTE: `arg2` needs to be shifted by 1!
mkOr :: Morphism -> Morphism -> Morphism
mkOr arg1 arg2 =
  MorphismCase
    Case
      { _caseLeftType = ObjectTerminal,
        _caseRightType = ObjectTerminal,
        _caseCodomainType = objectBool,
        _caseOn = arg1,
        _caseLeft =
          MorphismLambda
            Lambda
              { _lambdaVarType = ObjectTerminal,
                _lambdaBodyType = objectBool,
                _lambdaBody = morphismTrue
              },
        _caseRight =
          MorphismLambda
            Lambda
              { _lambdaVarType = ObjectTerminal,
                _lambdaBodyType = objectBool,
                _lambdaBody = arg2
              }
      }

objectLeftCase :: Case -> Object
objectLeftCase Case {..} =
  ObjectHom
    Hom
      { _homDomain = _caseLeftType,
        _homCodomain = _caseCodomainType
      }

objectRightCase :: Case -> Object
objectRightCase Case {..} =
  ObjectHom
    Hom
      { _homDomain = _caseRightType,
        _homCodomain = _caseCodomainType
      }

mkHoms :: [Object] -> Object -> Object
mkHoms argtys codty = case argtys of
  [] -> codty
  ty : tys ->
    ObjectHom $
      Hom
        { _homDomain = ty,
          _homCodomain = (mkHoms tys codty)
        }

objectBinop :: Binop -> Object
objectBinop op = case op ^. binopOpcode of
  OpAdd -> ObjectInteger
  OpSub -> ObjectInteger
  OpMul -> ObjectInteger
  OpDiv -> ObjectInteger
  OpMod -> ObjectInteger
  OpEq -> objectBool
  OpLt -> objectBool

mapVars :: (Int -> Var -> Morphism) -> Morphism -> Morphism
mapVars f m = go 0 m
  where
    go :: Int -> Morphism -> Morphism
    go k = \case
      MorphismAbsurd x -> MorphismAbsurd (over absurdValue (go k) x)
      MorphismUnit -> MorphismUnit
      MorphismLeft x -> MorphismLeft (over leftInjValue (go k) x)
      MorphismRight x -> MorphismRight (over rightInjValue (go k) x)
      MorphismCase x -> MorphismCase (over caseOn (go k) (over caseLeft (go k) (over caseRight (go k) x)))
      MorphismPair x -> MorphismPair (over pairLeft (go k) (over pairRight (go k) x))
      MorphismFirst x -> MorphismFirst (over firstValue (go k) x)
      MorphismSecond x -> MorphismSecond (over secondValue (go k) x)
      MorphismLambda x -> MorphismLambda (over lambdaBody (go (k + 1)) x)
      MorphismApplication x -> MorphismApplication (over applicationLeft (go k) (over applicationRight (go k) x))
      MorphismVar x -> f k x
      MorphismInteger i -> MorphismInteger i
      MorphismBinop x -> MorphismBinop (over binopLeft (go k) (over binopRight (go k) x))
      MorphismFail x -> MorphismFail x

shift :: Int -> Morphism -> Morphism
shift 0 m = m
shift n m = mapVars go m
  where
    go :: Int -> Var -> Morphism
    go k v@Var {..} | _varIndex >= k = MorphismVar v {_varIndex = _varIndex + n}
    go _ v = MorphismVar v

substs :: [Morphism] -> Morphism -> Morphism
substs env = mapVars go
  where
    n :: Int
    n = length env

    go :: Int -> Var -> Morphism
    go k Var {..} | _varIndex >= k && _varIndex < k + n = shift k (env !! (_varIndex - k))
    go _ v = MorphismVar v
