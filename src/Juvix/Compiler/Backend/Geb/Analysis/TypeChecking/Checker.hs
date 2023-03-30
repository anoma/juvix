module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Checker
  ( module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Checker,
  )
where

import Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Error
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra
import Juvix.Compiler.Backend.Geb.Language

type CheckingEnv = Context Object

check' :: Member (Error CheckingError) r => TypedMorphism -> Sem r TypedMorphism
check' tyMorph = do
  runReader (mempty @CheckingEnv) $ check (tyMorph ^. typedMorphism) (tyMorph ^. typedMorphismObject)
  return tyMorph

check :: Members '[Reader CheckingEnv, Error CheckingError] r => Morphism -> Object -> Sem r ()
check morph obj' = do
  ctx <- ask @CheckingEnv
  obj <- runReader ctx (inferObject morph)
  unless
    (obj == obj')
    ( throw $
        CheckingErrorTypeMismatch
          TypeMismatch
            { _typeMismatchMorphism = morph,
              _typeMismatchExpected = obj,
              _typeMismatchActual = obj'
            }
    )

checkSameType :: InferEffects r => [Morphism] -> Sem r ()
checkSameType = \case
  [] -> return ()
  (x : xs) -> do
    obj <- inferObject x
    checkListSameType xs obj

checkListSameType :: InferEffects r => [Morphism] -> Object -> Sem r ()
checkListSameType morphs obj = mapM_ (`check` obj) morphs

inferObject' :: Morphism -> Either CheckingError Object
inferObject' = run . runError . runReader mempty . inferObject @'[Reader CheckingEnv, Error CheckingError]

type InferEffects r = Members '[Reader CheckingEnv, Error CheckingError] r

inferObject :: InferEffects r => Morphism -> Sem r Object
inferObject = \case
  MorphismUnit -> return ObjectTerminal
  MorphismInteger {} -> return ObjectInteger
  MorphismAbsurd x -> inferObjectAbsurd x
  MorphismApplication app -> inferObjectApplication app
  MorphismPair pair -> inferObjectPair pair
  MorphismCase c -> inferObjectCase c
  MorphismFirst p -> inferObjectFirst p
  MorphismSecond p -> inferObjectSecond p
  MorphismLambda l -> inferObjectLambda l
  MorphismBinop op -> inferObjectBinop op
  MorphismVar v -> inferObjectVar v
  MorphismLeft a -> inferObjectLeft a
  MorphismRight b -> inferObjectRight b
  MorphismFail x -> return $ x ^. failureType

inferObjectAbsurd :: InferEffects r => Absurd -> Sem r Object
inferObjectAbsurd x = do
  check (x ^. absurdValue) (x ^. absurdType)
  return ObjectInitial

inferObjectApplication :: InferEffects r => Application -> Sem r Object
inferObjectApplication app = do
  let lType = app ^. applicationDomainType
      rType = app ^. applicationCodomainType
      homTy =
        ObjectHom $
          Hom {_homDomain = lType, _homCodomain = rType}
  check (app ^. applicationLeft) homTy
  check (app ^. applicationRight) lType
  return rType

inferObjectLambda :: InferEffects r => Lambda -> Sem r Object
inferObjectLambda l = do
  let aType = l ^. lambdaVarType
      bType = l ^. lambdaBodyType
  ctx <- ask @CheckingEnv
  local
    (const (Context.cons aType ctx))
    (check (l ^. lambdaBody) bType)
  return $
    ObjectHom $
      Hom
        { _homDomain = aType,
          _homCodomain = bType
        }

inferObjectPair :: InferEffects r => Pair -> Sem r Object
inferObjectPair pair = do
  let lType = pair ^. pairLeftType
      rType = pair ^. pairRightType
  check (pair ^. pairLeft) lType
  check (pair ^. pairRight) rType
  return $
    ObjectProduct
      Product
        { _productLeft = lType,
          _productRight = rType
        }

inferObjectCase :: InferEffects r => Case -> Sem r Object
inferObjectCase c = do
  let aType = c ^. caseLeftType
      bType = c ^. caseRightType
      vType =
        ObjectCoproduct $
          Coproduct
            { _coproductLeft = aType,
              _coproductRight = bType
            }
      cType = c ^. caseCodomainType
      leftType =
        ObjectHom $
          Hom
            { _homDomain = aType,
              _homCodomain = cType
            }
      rightType =
        ObjectHom $
          Hom
            { _homDomain = bType,
              _homCodomain = cType
            }
  check (c ^. caseOn) vType
  check (c ^. caseLeft) leftType
  check (c ^. caseRight) rightType
  return cType

inferObjectFirst :: InferEffects r => First -> Sem r Object
inferObjectFirst p = do
  let leftType = p ^. firstLeftType
      rightType = p ^. firstRightType
      pairType =
        ObjectProduct $
          Product
            { _productLeft = leftType,
              _productRight = rightType
            }
  check (p ^. firstValue) pairType
  return leftType

inferObjectSecond :: InferEffects r => Second -> Sem r Object
inferObjectSecond p = do
  let leftType = p ^. secondLeftType
      rightType = p ^. secondRightType
      pairType =
        ObjectProduct $
          Product
            { _productLeft = leftType,
              _productRight = rightType
            }
  check (p ^. secondValue) pairType
  return rightType

inferObjectVar :: InferEffects r => Var -> Sem r Object
inferObjectVar v = do
  ctx <- ask @CheckingEnv
  return $ Context.lookup (v ^. varIndex) ctx

inferObjectBinop :: InferEffects r => Binop -> Sem r Object
inferObjectBinop opApp = do
  let outTy = objectBinop opApp
      leftArg = opApp ^. binopLeft
      rightArg = opApp ^. binopRight
      args = [leftArg, rightArg]

  case opApp ^. binopOpcode of
    OpAdd -> do
      checkListSameType args ObjectInteger
      return outTy
    OpSub -> do
      checkListSameType args ObjectInteger
      return outTy
    OpMul -> do
      checkListSameType args ObjectInteger
      return outTy
    OpDiv -> do
      checkListSameType args ObjectInteger
      return outTy
    OpMod -> do
      checkListSameType args ObjectInteger
      return outTy
    OpLt -> do
      checkListSameType args ObjectInteger
      return outTy
    OpEq -> do
      checkSameType args
      return outTy

inferObjectLeft :: InferEffects r => LeftInj -> Sem r Object
inferObjectLeft LeftInj {..} = do
  check _leftInjValue _leftInjLeftType
  return $
    ObjectCoproduct $
      Coproduct
        { _coproductLeft = _leftInjLeftType,
          _coproductRight = _leftInjRightType
        }

inferObjectRight :: InferEffects r => RightInj -> Sem r Object
inferObjectRight RightInj {..} = do
  check _rightInjValue _rightInjRightType
  return $
    ObjectCoproduct $
      Coproduct
        { _coproductLeft = _rightInjLeftType,
          _coproductRight = _rightInjRightType
        }
