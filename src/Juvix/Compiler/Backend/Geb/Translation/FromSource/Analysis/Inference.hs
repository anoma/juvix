module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Prelude

inferObject' :: Geb.Morphism -> Either JuvixError Geb.Object
inferObject' = run . runError . inferObject Context.empty Nothing

inferObject ::
  Members '[Error JuvixError] r =>
  Context Geb.Object ->
  Maybe Geb.Object ->
  Geb.Morphism ->
  Sem r Geb.Object
inferObject ctx tyInfo = \case
  Geb.MorphismUnit -> return Geb.ObjectTerminal
  Geb.MorphismInteger {} -> return Geb.ObjectInteger
  Geb.MorphismAbsurd x -> inferObject ctx tyInfo x
  Geb.MorphismPair pair -> do
    let lType = pair ^. Geb.pairLeftType
        rType = pair ^. Geb.pairRightType
    lTy <- inferObject ctx (Just lType) (pair ^. Geb.pairLeft)
    rTy <- inferObject ctx (Just rType) (pair ^. Geb.pairRight)
    unless
      (lTy == lType)
      (error "Type mismatch: left type on a pair")
    unless
      (rTy == rType)
      (error "Type mismatch: right type on a pair")
    return $
      Geb.ObjectProduct
        Geb.Product
          { _productLeft = lTy,
            _productRight = rTy
          }
  Geb.MorphismCase c ->
    -- TODO check leaves
    return $ c ^. Geb.caseCodomainType
  Geb.MorphismFirst p -> do
    let leftType = p ^. Geb.firstLeftType
    ty <- inferObject ctx (Just leftType) (p ^. Geb.firstValue)
    unless (ty == leftType) (error "Type mismatch")
    return ty
  Geb.MorphismSecond p -> do
    let rightType = p ^. Geb.secondRightType
    ty <- inferObject ctx (Just rightType) (p ^. Geb.secondValue)
    unless (ty == rightType) (error "Type mismatch")
    return ty
  Geb.MorphismLambda l -> do
    let bodyTy = l ^. Geb.lambdaBodyType
    bTy <-
      inferObject
        (Context.cons (l ^. Geb.lambdaVarType) ctx)
        (Just bodyTy)
        (l ^. Geb.lambdaBody)
    unless
      (bTy == bodyTy)
      (error "Type mismatch: body of the lambda")
    return $
      Geb.ObjectHom $
        Geb.Hom
          { _homDomain = l ^. Geb.lambdaVarType,
            _homCodomain = l ^. Geb.lambdaBodyType
          }
  Geb.MorphismApplication app -> do
    lTy <- inferObject ctx Nothing (app ^. Geb.applicationLeft)
    rTy <- inferObject ctx Nothing (app ^. Geb.applicationRight)
    case lTy of
      Geb.ObjectHom h -> do
        unless
          (h ^. Geb.homDomain == rTy)
          (error "Type mismatch: domain of the function and the argument")
        return $ h ^. Geb.homCodomain
      _ -> error "Left side of the application should be a function"
  Geb.MorphismBinop op -> do
    let outTy = objectBinop op
    aTy <- inferObject ctx (Just outTy) (op ^. Geb.binopLeft)
    bTy <- inferObject ctx (Just outTy) (op ^. Geb.binopRight)
    unless
      (aTy == bTy)
      (error "Arguments of a binary operation should have the same type")
    return outTy
  Geb.MorphismVar v -> do
    let envTy = Context.lookup (v ^. Geb.varIndex) ctx
    unless (Just envTy == tyInfo) (error "Type mismatch: variable")
    return envTy
  -- FIXME: Once https://github.com/anoma/geb/issues/53 is fixed, we should
  -- modify the following cases, and use the type information provided.
  Geb.MorphismLeft a -> do
    case tyInfo of
      Just cTy@(Geb.ObjectCoproduct coprod) -> do
        let leftTy = coprod ^. Geb.coproductLeft
        aTy <- inferObject ctx (Just leftTy) a
        unless
          (aTy == leftTy)
          (error "Type mismatch: left morphism")
        return cTy
      Just _ -> error "Expected a coproduct object for a left morphism."
      Nothing ->
        error $
          lackOfInformation
            <> " on the left morphism "
            <> show a
  Geb.MorphismRight b ->
    case tyInfo of
      Just cTy@(Geb.ObjectCoproduct coprod) -> do
        let rightTy = coprod ^. Geb.coproductRight
        bTy <- inferObject ctx (Just rightTy) b
        unless
          (bTy == rightTy)
          (error "Type mismatch: right morphism")
        return cTy
      Just _ -> error "Expected a coproduct object for a right morphism."
      Nothing -> error $ lackOfInformation <> " on a right morphism"

lackOfInformation :: Text
lackOfInformation = "Not enough information to infer the type"

objectBinop :: Geb.Binop -> Geb.Object
objectBinop op = case op ^. Geb.binopOpcode of
  Geb.OpAdd -> Geb.ObjectInteger
  Geb.OpSub -> Geb.ObjectInteger
  Geb.OpMul -> Geb.ObjectInteger
  Geb.OpDiv -> Geb.ObjectInteger
  Geb.OpMod -> Geb.ObjectInteger
  Geb.OpEq -> Geb.objectBool
  Geb.OpLt -> Geb.objectBool
