module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking
  ( module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking,
    module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Data.Types,
    module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Error,
  )
where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Data.Types
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Error

check' :: TypedMorphism -> Either JuvixError TypedMorphism
check' tyMorph = do
  -- let checkOut = run . runError (check (tyMorph ^. typedMorphism) (tyMorph ^. typedMorphismObject))
  case (run . runError $ check (tyMorph ^. typedMorphism) (tyMorph ^. typedMorphismObject)) of
    Left err -> return (JuvixError err)
    Right _ -> tyMorph

  -- return undefined

check :: Members '[Error CheckingError] r => Morphism -> Object -> Sem r ()
check morph obj' = do
  obj <-
    runReader
      (defaultInferenceEnv {_inferenceEnvTypeInfo = Just obj'})
      (inferObject morph)
  unless
    (obj == obj')
    ( throw $ CheckingErrorTypeMismatch
          TypeMismatch
            { _typeMismatchMorphism = morph,
              _typeMismatchExpected = obj,
              _typeMismatchActual = obj
            }
    )

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
  MorphismAbsurd x -> inferObject x
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
  -- FIXME: Once https://github.com/anoma/geb/issues/53 is fixed, we have
  -- to update Left,Right cases to use their type information.

inferObjectApplication :: InferEffects r => Application -> Sem r Object
inferObjectApplication app = do
  let domainTy = app ^. applicationDomainType
      codomainTy = app ^. applicationCodomainType
      homTy =
        ObjectHom $
          Hom {_homDomain = domainTy, _homCodomain = codomainTy}
  funTy <-
    local
      (over inferenceEnvTypeInfo (const (Just homTy)))
      (inferObject (app ^. applicationLeft))
  argTy <-
    local
      (over inferenceEnvTypeInfo (const (Just domainTy)))
      (inferObject (app ^. applicationRight))
  case funTy of
    ObjectHom h -> do
      unless
        (h ^. homDomain == argTy)
        ( throw $ CheckingErrorWrongObject
              WrongObject
                { _wrongObjectExpected = Just $ h ^. homDomain,
                  _wrongObjectActual = Just argTy,
                  _wrongObjectMorphism = app ^. applicationRight,
                  _wrongObjectMessage = "Type mismatch: domain of the function and the argument."
                }
        )
      return $ h ^. homCodomain
    obj ->
      ( throw
          $ CheckingErrorWrongObject
            WrongObject
              { _wrongObjectMessage = "Left side of the application should be a function",
                _wrongObjectExpected = Nothing,
                _wrongObjectActual = Just obj,
                _wrongObjectMorphism = app ^. applicationLeft
              }
      )

inferObjectPair :: InferEffects r => Pair -> Sem r Object
inferObjectPair pair = do
  let lType = pair ^. pairLeftType
      rType = pair ^. pairRightType
  infObjLeft <-
    local
      (over inferenceEnvTypeInfo (const (Just lType)))
      (inferObject (pair ^. pairLeft))
  unless
    (infObjLeft == lType)
    (errorInferObject "Type mismatch: left type on a pair")
  infRightType <-
    local
      (over inferenceEnvTypeInfo (const (Just rType)))
      (inferObject (pair ^. pairRight))
  unless
    (infRightType == rType)
    (errorInferObject "Type mismatch: right type on a pair")
  return $
    ObjectProduct
      Product
        { _productLeft = infObjLeft,
          _productRight = infRightType
        }

inferObjectCase :: InferEffects r => Case -> Sem r Object
inferObjectCase c = return $ c ^. caseCodomainType

inferObjectFirst :: InferEffects r => First -> Sem r Object
inferObjectFirst p = do
  let leftType = p ^. firstLeftType
  ty <-
    local
      (over inferenceEnvTypeInfo (const (Just leftType)))
      (inferObject (p ^. firstValue))
  unless (ty == leftType) (errorInferObject "Type mismatch")
  return ty

inferObjectSecond :: InferEffects r => Second -> Sem r Object
inferObjectSecond p = do
  let rightType = p ^. secondRightType
  ty <-
    local (over inferenceEnvTypeInfo (const (Just rightType))) $
      inferObject (p ^. secondValue)
  unless (ty == rightType) (errorInferObject "Type mismatch")
  return ty

inferObjectLambda :: InferEffects r => Lambda -> Sem r Object
inferObjectLambda l = do
  let bodyTy = l ^. lambdaBodyType
  ctx <- asks (^. inferenceEnvContext)
  bTy <-
    local
      ( const
          ( InferenceEnv
              { _inferenceEnvContext = Context.cons (l ^. lambdaVarType) ctx,
                _inferenceEnvTypeInfo = Just bodyTy
              }
          )
      )
      (inferObject (l ^. lambdaBody))
  unless
    (bTy == bodyTy)
    (errorInferObject "Type mismatch: body of the lambda")
  return $
    ObjectHom $
      Hom
        { _homDomain = l ^. lambdaVarType,
          _homCodomain = l ^. lambdaBodyType
        }

inferObjectBinop :: InferEffects r => Binop -> Sem r Object
inferObjectBinop op = do
  let outTy = objectBinop op
  aTy <-
    local
      (over inferenceEnvTypeInfo (const (Just outTy)))
      (inferObject (op ^. binopLeft))
  bTy <-
    local
      (over inferenceEnvTypeInfo (const (Just outTy)))
      (inferObject (op ^. binopRight))
  unless
    (aTy == bTy)
    (errorInferObject "Arguments of a binary operation should have the same type")
  return outTy

inferObjectVar :: InferEffects r => Var -> Sem r Object
inferObjectVar v = do
  ctx <- asks (^. inferenceEnvContext)
  let varTy = Context.lookup (v ^. varIndex) ctx
  tyInfo' <- asks (^. inferenceEnvTypeInfo)
  case tyInfo' of
    Nothing ->
      throw
        $ CheckingErrorLackOfInformation
          LackOfInformation
            { _lackOfInformationMorphism = Just (MorphismVar v),
              _lacOfInformationHelperObject = Nothing,
              _lackOfInformationMessage = "Expected type information for variable"
            }
    Just tyInfo -> do
      unless
        (varTy == tyInfo)
        ( throw $ CheckingErrorTypeMismatch
              TypeMismatch
                { _typeMismatchExpected = tyInfo,
                  _typeMismatchActual = varTy,
                  _typeMismatchMorphism = MorphismVar v
                }
        )
      return varTy

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
          ( throw
              $ CheckingErrorTypeMismatch
                TypeMismatch
                  { _typeMismatchExpected = aTy,
                    _typeMismatchActual = leftTy,
                    _typeMismatchMorphism = MorphismLeft a
                  }
          )
        return cTy
      Just ty ->
        throw
          $ CheckingErrorWrongObject
            WrongObject
              { _wrongObjectExpected = Nothing,
                _wrongObjectActual = Just ty,
                _wrongObjectMorphism = MorphismLeft a,
                _wrongObjectMessage = "Expected a coproduct object for a left morphism."
              }
      Nothing ->
        throw
          $ CheckingErrorLackOfInformation
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
        ( throw $ CheckingErrorTypeMismatch
              TypeMismatch
                { _typeMismatchExpected = bTy,
                  _typeMismatchActual = rightTy,
                  _typeMismatchMorphism = MorphismRight bMorph
                }
        )
      return cTy
    Just ty ->
      throw
        $ CheckingErrorWrongObject
          WrongObject
            { _wrongObjectExpected = Nothing,
              _wrongObjectActual = Just ty,
              _wrongObjectMorphism = MorphismRight bMorph,
              _wrongObjectMessage = "Expected a coproduct object for a right morphism."
            }
    Nothing ->
      throw
        $ CheckingErrorLackOfInformation
          LackOfInformation
            { _lackOfInformationMorphism = Just (MorphismRight bMorph),
              _lacOfInformationHelperObject = tyInfo,
              _lackOfInformationMessage = "on a right morphism"
            }

errorInferObject :: Text -> a
errorInferObject = error . ("infer: " <>)

lackOfInformation :: Text
lackOfInformation = "Not enough information to inferObject the type"
