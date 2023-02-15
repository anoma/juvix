module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking
  ( module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking,
    module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Data.Types,
    module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Error,
  )
where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Data.Types
import Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.TypeChecking.Error

check' :: TypedMorphism -> Either JuvixError Object
check' = run . runError . check

check :: Members '[Error JuvixError] r => TypedMorphism -> Sem r Object
check TypedMorphism {..} = do
  infObj <-
    runReader
      (defaultInferenceEnv {_inferenceEnvTypeInfo = Just _typedMorphismObject})
      (infer _typedMorphism)
  if
      | infObj == _typedMorphismObject -> return infObj
      | otherwise ->
          throw
            . JuvixError
            $ CheckingErrorTypeMismatch
              TypeMismatch
                { _typeMismatchMorphism = _typedMorphism,
                  _typeMismatchExpected = _typedMorphismObject,
                  _typeMismatchActual = infObj
                }

infer' :: Morphism -> Either JuvixError Object
infer' = run . runError . runReader defaultInferenceEnv . infer

infer ::
  Members '[Reader InferenceEnv, Error JuvixError] r =>
  Morphism ->
  Sem r Object
infer = \case
  MorphismUnit -> return ObjectTerminal
  MorphismInteger {} -> return ObjectInteger
  MorphismAbsurd x -> infer x
  MorphismPair pair -> do
    let lType = pair ^. pairLeftType
        rType = pair ^. pairRightType
    infObjLeft <-
      local
        (over inferenceEnvTypeInfo (const (Just lType)))
        (infer (pair ^. pairLeft))
    unless
      (infObjLeft == lType)
      (errorInferObject "Type mismatch: left type on a pair")
    infRightType <-
      local
        (over inferenceEnvTypeInfo (const (Just rType)))
        (infer (pair ^. pairRight))
    unless
      (infRightType == rType)
      (errorInferObject "Type mismatch: right type on a pair")
    return $
      ObjectProduct
        Product
          { _productLeft = infObjLeft,
            _productRight = infRightType
          }
  MorphismCase c -> do
    -- TODO check leaves
    return $ c ^. caseCodomainType
  MorphismFirst p -> do
    let leftType = p ^. firstLeftType
    ty <-
      local
        (over inferenceEnvTypeInfo (const (Just leftType)))
        (infer (p ^. firstValue))
    unless (ty == leftType) (errorInferObject "Type mismatch")
    return ty
  MorphismSecond p -> do
    let rightType = p ^. secondRightType
    ty <-
      local (over inferenceEnvTypeInfo (const (Just rightType))) $
        infer (p ^. secondValue)
    unless (ty == rightType) (errorInferObject "Type mismatch")
    return ty
  MorphismLambda l -> do
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
        (infer (l ^. lambdaBody))
    unless
      (bTy == bodyTy)
      (errorInferObject "Type mismatch: body of the lambda")
    return $
      ObjectHom $
        Hom
          { _homDomain = l ^. lambdaVarType,
            _homCodomain = l ^. lambdaBodyType
          }
  MorphismApplication app -> do
    let leftTy = app ^. applicationDomainType -- A -> B
        rightTy = app ^. applicationCodomainType -- A
        homTy = ObjectHom $ Hom {_homDomain = rightTy, _homCodomain = leftTy}
    lTy <-
      local
        (over inferenceEnvTypeInfo (const (Just homTy)))
        ( infer
            (app ^. applicationLeft)
        )
    rTy <-
      local
        (over inferenceEnvTypeInfo (const (Just rightTy)))
        (infer (app ^. applicationRight))
    case lTy of
      ObjectHom h -> do
        unless
          (h ^. homDomain == rTy)
          (errorInferObject "Type mismatch: domain of the function and the argument")
        return $ h ^. homCodomain
      _ -> errorInferObject "Left side of the application should be a function"
  MorphismBinop op -> do
    let outTy = objectBinop op
    aTy <-
      local
        (over inferenceEnvTypeInfo (const (Just outTy)))
        (infer (op ^. binopLeft))
    bTy <-
      local
        (over inferenceEnvTypeInfo (const (Just outTy)))
        (infer (op ^. binopRight))
    unless
      (aTy == bTy)
      (errorInferObject "Arguments of a binary operation should have the same type")
    return outTy
  MorphismVar v -> do
    ctx <- asks (^. inferenceEnvContext)
    let varTy = Context.lookup (v ^. varIndex) ctx
    tyInfo <-
      fromMaybe (error "Expected type")
        <$> asks (^. inferenceEnvTypeInfo)
    unless
      (varTy == tyInfo)
      ( errorInferObject $
          "\nType mismatch: variable "
            <> ppPrint (MorphismVar v)
            <> " has type:\n"
            <> ppPrint varTy
            <> " but it's expected to have type:\n"
            <> ppPrint tyInfo
      )
    return varTy
  -- FIXME: Once https://github.com/anoma/geb/issues/53 is fixed, we should
  -- modify the following cases, and use the type information provided.
  MorphismLeft a -> do
    tyInfo <- asks (^. inferenceEnvTypeInfo)
    case tyInfo of
      Just cTy@(ObjectCoproduct coprod) -> do
        let leftTy = coprod ^. coproductLeft
        aTy <-
          local
            (over inferenceEnvTypeInfo (const (Just leftTy)))
            (infer a)
        unless
          (aTy == leftTy)
          (errorInferObject "Type mismatch: left morphism")
        return cTy
      Just _ -> errorInferObject "Expected a coproduct object for a left morphism."
      Nothing ->
        errorInferObject $
          lackOfInformation
            <> " on the left morphism:\n\t"
            <> ppPrint a
  MorphismRight b -> do
    tyInfo <- asks (^. inferenceEnvTypeInfo)
    case tyInfo of
      Just cTy@(ObjectCoproduct coprod) -> do
        let rightTy = coprod ^. coproductRight
        bTy <-
          local
            (over inferenceEnvTypeInfo (const (Just rightTy)))
            (infer b)
        unless
          (bTy == rightTy)
          (errorInferObject "Type mismatch: right morphism")
        return cTy
      Just _ -> errorInferObject "Expected a coproduct object for a right morphism."
      Nothing -> errorInferObject $ lackOfInformation <> " on a right morphism"

errorInferObject :: Text -> a
errorInferObject = error . ("infer: " <>)

lackOfInformation :: Text
lackOfInformation = "Not enough information to infer the type"
