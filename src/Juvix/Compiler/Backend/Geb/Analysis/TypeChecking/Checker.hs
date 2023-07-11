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
  checkTypesEqual obj obj'

checkTypesEqual :: Members '[Reader CheckingEnv, Error CheckingError] r => Object -> Object -> Sem r ()
checkTypesEqual obj obj' =
  unless
    (obj == obj')
    ( throw $
        CheckingErrorTypeMismatch
          TypeMismatch
            { _typeMismatchExpected = obj,
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
  homTy <- inferObject (app ^. applicationLeft)
  lType <- inferObject (app ^. applicationRight)
  case homTy of
    ObjectHom Hom {..} -> do
      checkTypesEqual _homDomain lType
      return _homCodomain
    _ ->
      throw $
        CheckingErrorExpectedType
          ExpectedType
            { _expectedTypeObject = homTy,
              _expectedTypeKind = "hom object"
            }

inferObjectLambda :: InferEffects r => Lambda -> Sem r Object
inferObjectLambda l = do
  let aType = l ^. lambdaVarType
  ctx <- ask @CheckingEnv
  bType <-
    local
      (const (Context.cons aType ctx))
      (inferObject (l ^. lambdaBody))
  return $
    ObjectHom $
      Hom
        { _homDomain = aType,
          _homCodomain = bType
        }

inferObjectPair :: InferEffects r => Pair -> Sem r Object
inferObjectPair pair = do
  lType <- inferObject (pair ^. pairLeft)
  rType <- inferObject (pair ^. pairRight)
  return $
    ObjectProduct
      Product
        { _productLeft = lType,
          _productRight = rType
        }

inferObjectCase :: InferEffects r => Case -> Sem r Object
inferObjectCase c = do
  vType <- inferObject (c ^. caseOn)
  case vType of
    ObjectCoproduct Coproduct {..} -> do
      ctx <- ask @CheckingEnv
      leftType <-
        local
          (const (Context.cons _coproductLeft ctx))
          (inferObject (c ^. caseLeft))
      rightType <-
        local
          (const (Context.cons _coproductRight ctx))
          (inferObject (c ^. caseRight))
      checkTypesEqual leftType rightType
      return leftType
    _ ->
      throw $
        CheckingErrorExpectedType
          ExpectedType
            { _expectedTypeObject = vType,
              _expectedTypeKind = "coproduct"
            }

inferObjectFirst :: InferEffects r => First -> Sem r Object
inferObjectFirst p = do
  pairType <- inferObject (p ^. firstValue)
  case pairType of
    ObjectProduct Product {..} ->
      return _productLeft
    _ ->
      throw $
        CheckingErrorExpectedType
          ExpectedType
            { _expectedTypeObject = pairType,
              _expectedTypeKind = "product"
            }

inferObjectSecond :: InferEffects r => Second -> Sem r Object
inferObjectSecond p = do
  pairType <- inferObject (p ^. secondValue)
  case pairType of
    ObjectProduct Product {..} ->
      return _productRight
    _ ->
      throw $
        CheckingErrorExpectedType
          ExpectedType
            { _expectedTypeObject = pairType,
              _expectedTypeKind = "product"
            }

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
  lType <- inferObject _leftInjValue
  return $
    ObjectCoproduct $
      Coproduct
        { _coproductLeft = lType,
          _coproductRight = _leftInjRightType
        }

inferObjectRight :: InferEffects r => RightInj -> Sem r Object
inferObjectRight RightInj {..} = do
  rType <- inferObject _rightInjValue
  return $
    ObjectCoproduct $
      Coproduct
        { _coproductLeft = _rightInjLeftType,
          _coproductRight = rType
        }
