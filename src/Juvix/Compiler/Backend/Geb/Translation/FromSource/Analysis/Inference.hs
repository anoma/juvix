module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference where

import Juvix.Compiler.Backend.Geb.Data.Context as Context
import Juvix.Compiler.Backend.Geb.Extra qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Prelude

inferObject' :: Geb.Morphism -> Either JuvixError Geb.Object
inferObject' = run . runError . inferObject Context.empty

inferObject ::
  Members '[Error JuvixError] r =>
  Context Geb.Object ->
  Geb.Morphism ->
  Sem r Geb.Object
inferObject ctx = \case
  Geb.MorphismUnit -> return Geb.ObjectTerminal
  Geb.MorphismInteger {} -> return Geb.ObjectInteger
  Geb.MorphismAbsurd x -> inferObject ctx x
  Geb.MorphismPair pair -> do
    lTy <- inferObject ctx (pair ^. Geb.pairLeft)
    rTy <- inferObject ctx (pair ^. Geb.pairRight)
    unless
      (lTy == pair ^. Geb.pairLeftType)
      (error "Type mismatch: left type on a pair")
    unless
      (rTy == pair ^. Geb.pairRightType)
      (error "Type mismatch: right type on a pair")
    return $
      Geb.ObjectProduct
        Geb.Product
          { _productLeft = lTy,
            _productRight = rTy
          }
  Geb.MorphismCase c -> return $ c ^. Geb.caseCodomainType
  Geb.MorphismFirst p -> do
    ty <- inferObject ctx (p ^. Geb.firstValue)
    unless (ty == p ^. Geb.firstLeftType) (error "Type mismatch")
    return ty
  Geb.MorphismSecond p -> do
    ty <- inferObject ctx (p ^. Geb.secondValue)
    unless (ty == p ^. Geb.secondRightType) (error "Type mismatch")
    return ty
  Geb.MorphismLambda l -> do
    bTy <-
      inferObject
        (Context.cons (l ^. Geb.lambdaVarType) ctx)
        (l ^. Geb.lambdaBody)
    unless
      (bTy == l ^. Geb.lambdaBodyType)
      (error "Type mismatch: body of the lambda")
    return $
      Geb.ObjectHom $
        Geb.Hom
          { _homDomain = l ^. Geb.lambdaVarType,
            _homCodomain = l ^. Geb.lambdaBodyType
          }
  Geb.MorphismApplication app -> do
    lTy <- inferObject ctx (app ^. Geb.applicationLeft)
    rTy <- inferObject ctx (app ^. Geb.applicationRight)
    case lTy of
      Geb.ObjectHom h -> do
        unless
          (h ^. Geb.homDomain == rTy)
          (error "Type mismatch: domain of the function and the argument")
        return $ h ^. Geb.homCodomain
      _ -> error "Left side of the application should be a function"
  Geb.MorphismBinop op -> do
    aTy <- inferObject ctx (op ^. Geb.binopLeft)
    bTy <- inferObject ctx (op ^. Geb.binopRight)
    unless
      (aTy == bTy)
      (error "Arguments of a binary operation should have the same type")
    return $ objectBinop op
  Geb.MorphismVar v -> return $ Context.lookup (v ^. Geb.varIndex) ctx
  -- FIXME: Once https://github.com/anoma/geb/issues/53 is fixed, we should
  -- modify the following cases.
  Geb.MorphismLeft {} -> error $ lackOfInformation <> " on a left morphism"
  Geb.MorphismRight {} -> error $ lackOfInformation <> " on a right morphism"

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
