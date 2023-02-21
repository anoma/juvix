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
morphismTrue = MorphismLeft MorphismUnit

morphismFalse :: Morphism
morphismFalse = MorphismRight MorphismUnit

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
  ty : tys -> ObjectHom $ Hom ty (mkHoms tys codty)

objectBinop :: Binop -> Object
objectBinop op = case op ^. binopOpcode of
  OpAdd -> ObjectInteger
  OpSub -> ObjectInteger
  OpMul -> ObjectInteger
  OpDiv -> ObjectInteger
  OpMod -> ObjectInteger
  OpEq -> objectBool
  OpLt -> objectBool
