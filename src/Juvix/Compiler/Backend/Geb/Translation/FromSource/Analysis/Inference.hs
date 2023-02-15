module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra
import Juvix.Compiler.Backend.Geb.Language
import Juvix.Compiler.Backend.Geb.Pretty

-- Context.empty Nothing

data InferenceEnv = InferenceEnv
  { -- | The context of the term being inferred.
    _inferenceEnvContext :: Context Object,
    -- | A Geb object to help the inference process.
    -- This is needed because some morphisms lack of type information.
    -- For example, the case of the left injection of a coproduct.
    _inferenceEnvTypeInfo :: Maybe Object
  }
  deriving stock (Show, Generic)

makeLenses ''InferenceEnv

defaultInferenceEnv :: InferenceEnv
defaultInferenceEnv =
  InferenceEnv
    { _inferenceEnvContext = Context.empty,
      _inferenceEnvTypeInfo = Nothing
    }

check = undefined -- TODO

inferObject' :: Morphism -> Either JuvixError Object
inferObject' m =
  run . runError $ runReader defaultInferenceEnv (inferObject m)

inferObject ::
  Members '[Reader InferenceEnv, Error JuvixError] r =>
  Morphism ->
  Sem r Object
inferObject = \case
  MorphismUnit -> return ObjectTerminal
  MorphismInteger {} -> return ObjectInteger
  MorphismAbsurd x -> inferObject x
  MorphismPair pair -> do
    let lType = pair ^. pairLeftType
        rType = pair ^. pairRightType
    infLeftType <-
      local
        (over inferenceEnvTypeInfo (const (Just lType)))
        (inferObject (pair ^. pairLeft))
    unless
      (infLeftType == lType)
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
          { _productLeft = infLeftType,
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
        (inferObject (p ^. firstValue))
    unless (ty == leftType) (errorInferObject "Type mismatch")
    return ty
  MorphismSecond p -> do
    let rightType = p ^. secondRightType
    ty <-
      local (over inferenceEnvTypeInfo (const (Just rightType))) $
        inferObject (p ^. secondValue)
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
  MorphismApplication app -> do
    let leftTy = app ^. applicationDomainType -- A -> B
        rightTy = app ^. applicationCodomainType -- A
        homTy = ObjectHom $ Hom {_homDomain = rightTy, _homCodomain = leftTy}
    lTy <-
      local
        (over inferenceEnvTypeInfo (const (Just homTy)))
        ( inferObject
            (app ^. applicationLeft)
        )
    rTy <-
      local
        (over inferenceEnvTypeInfo (const (Just rightTy)))
        (inferObject (app ^. applicationRight))
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
        (inferObject (op ^. binopLeft))
    bTy <-
      local
        (over inferenceEnvTypeInfo (const (Just outTy)))
        (inferObject (op ^. binopRight))
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
            (inferObject a)
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
            (inferObject b)
        unless
          (bTy == rightTy)
          (errorInferObject "Type mismatch: right morphism")
        return cTy
      Just _ -> errorInferObject "Expected a coproduct object for a right morphism."
      Nothing -> errorInferObject $ lackOfInformation <> " on a right morphism"

errorInferObject :: Text -> a
errorInferObject = error . ("inferObject: " <>)

lackOfInformation :: Text
lackOfInformation = "Not enough information to infer the type"
