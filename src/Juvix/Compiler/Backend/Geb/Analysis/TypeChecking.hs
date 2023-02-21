module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking
  ( module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking,
    module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Data.Types,
    module Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Error,
  )
where

import Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Data.Types
import Juvix.Compiler.Backend.Geb.Analysis.TypeChecking.Error
import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra
import Juvix.Compiler.Backend.Geb.Language

check' :: Member (Error CheckingError) r => TypedMorphism -> Sem r TypedMorphism
check' tyMorph = do
  runReader defaultInferenceEnv $ check (tyMorph ^. typedMorphism) (tyMorph ^. typedMorphismObject)
  return tyMorph

check :: Members '[Reader InferenceEnv, Error CheckingError] r => Morphism -> Object -> Sem r ()
check morph obj' = do
  obj <-
    runReader
      (defaultInferenceEnv {_inferenceEnvTypeInfo = Just obj'})
      (inferObject morph)
  unless
    (obj == obj')
    ( throw $
        CheckingErrorTypeMismatch
          TypeMismatch
            { _typeMismatchMorphism = morph,
              _typeMismatchExpected = obj,
              _typeMismatchActual = obj
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
inferObject' = run . runError . runReader defaultInferenceEnv . inferObject

type InferEffects r = Members '[Reader InferenceEnv, Error CheckingError] r

inferObject ::
  Members '[Reader InferenceEnv, Error CheckingError] r =>
  Morphism ->
  Sem r Object
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

-- FIXME: Depends on fixing anoma/geb#53
inferObjectAbsurd :: InferEffects r => Morphism -> Sem r Object
inferObjectAbsurd x =
  throw
    ( CheckingErrorLackOfInformation
        LackOfInformation
          { _lackOfInformationMorphism = Just x,
            _lacOfInformationHelperObject = Nothing,
            _lackOfInformationMessage = "Absurd"
          }
    )

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
  ctx <- asks (^. inferenceEnvContext)
  local
    ( const
        ( InferenceEnv
            { _inferenceEnvContext = Context.cons aType ctx,
              _inferenceEnvTypeInfo = Just aType
            }
        )
    )
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
  ctx <- asks (^. inferenceEnvContext)
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

-- FIXME: Once https://github.com/anoma/geb/issues/53 is fixed,
-- Update: inferObjectLeft and inferObjectRight to use the same code
inferObjectLeft :: InferEffects r => Morphism -> Sem r Object
inferObjectLeft a =
  do
    tyInfo <- asks (^. inferenceEnvTypeInfo)
    case tyInfo of
      Just cTy@(ObjectCoproduct coprod) -> do
        let leftTy = coprod ^. coproductLeft
        aTy <-
          local
            (over inferenceEnvTypeInfo (const (Just leftTy)))
            (inferObject a)
        unless
          (aTy == leftTy)
          ( throw $
              CheckingErrorTypeMismatch
                TypeMismatch
                  { _typeMismatchExpected = aTy,
                    _typeMismatchActual = leftTy,
                    _typeMismatchMorphism = MorphismLeft a
                  }
          )
        return cTy
      Just ty ->
        throw $
          CheckingErrorWrongObject
            WrongObject
              { _wrongObjectExpected = Nothing,
                _wrongObjectActual = Just ty,
                _wrongObjectMorphism = MorphismLeft a,
                _wrongObjectMessage = "Expected a coproduct object for a left morphism."
              }
      Nothing ->
        throw $
          CheckingErrorLackOfInformation
            LackOfInformation
              { _lackOfInformationMorphism = Just (MorphismLeft a),
                _lacOfInformationHelperObject = tyInfo,
                _lackOfInformationMessage = "on a left morphism"
              }

inferObjectRight :: InferEffects r => Morphism -> Sem r Object
inferObjectRight bMorph = do
  tyInfo <- asks (^. inferenceEnvTypeInfo)
  case tyInfo of
    Just cTy@(ObjectCoproduct coprod) -> do
      let rightTy = coprod ^. coproductRight
      bTy <-
        local
          (over inferenceEnvTypeInfo (const (Just rightTy)))
          (inferObject bMorph)
      unless
        (bTy == rightTy)
        ( throw $
            CheckingErrorTypeMismatch
              TypeMismatch
                { _typeMismatchExpected = bTy,
                  _typeMismatchActual = rightTy,
                  _typeMismatchMorphism = MorphismRight bMorph
                }
        )
      return cTy
    Just ty ->
      throw $
        CheckingErrorWrongObject
          WrongObject
            { _wrongObjectExpected = Nothing,
              _wrongObjectActual = Just ty,
              _wrongObjectMorphism = MorphismRight bMorph,
              _wrongObjectMessage = "Expected a coproduct object for a right morphism."
            }
    Nothing ->
      throw $
        CheckingErrorLackOfInformation
          LackOfInformation
            { _lackOfInformationMorphism = Just (MorphismRight bMorph),
              _lacOfInformationHelperObject = tyInfo,
              _lackOfInformationMessage = "on a right morphism"
            }
