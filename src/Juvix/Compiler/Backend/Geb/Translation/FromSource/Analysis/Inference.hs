module Juvix.Compiler.Backend.Geb.Translation.FromSource.Analysis.Inference where

import Juvix.Compiler.Backend.Geb.Extra qualified as Geb
import Juvix.Compiler.Backend.Geb.Language qualified as Geb
import Juvix.Prelude

objectBinop :: Geb.Binop -> Geb.Object
objectBinop op = case op ^. Geb.binopOpcode of
  Geb.OpAdd -> Geb.ObjectInteger
  Geb.OpSub -> Geb.ObjectInteger
  Geb.OpMul -> Geb.ObjectInteger
  Geb.OpDiv -> Geb.ObjectInteger
  Geb.OpMod -> Geb.ObjectInteger
  Geb.OpEq -> Geb.objectBool
  Geb.OpLt -> Geb.objectBool

inferObject ::
  Members '[Error JuvixError] r =>
  Geb.Morphism ->
  Sem r Geb.Object
inferObject = \case
  Geb.MorphismUnit -> return Geb.ObjectTerminal
  Geb.MorphismInteger {} -> return Geb.ObjectInteger
  Geb.MorphismAbsurd x -> inferObject x
  Geb.MorphismPair pair -> do
    lTy <- inferObject (pair ^. Geb.pairLeft)
    rTy <- inferObject (pair ^. Geb.pairRight)
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
    ty <- inferObject (p ^. Geb.firstValue)
    unless (ty == p ^. Geb.firstLeftType) (error "Type mismatch")
    return ty
  Geb.MorphismSecond p -> do
    ty <- inferObject (p ^. Geb.secondValue)
    unless (ty == p ^. Geb.secondRightType) (error "Type mismatch")
    return ty
  Geb.MorphismLambda l ->
    return $
      Geb.ObjectHom $
        Geb.Hom
          { _homDomain = l ^. Geb.lambdaVarType,
            _homCodomain = l ^. Geb.lambdaBodyType
          }
  Geb.MorphismApplication app -> do
    lTy <- inferObject (app ^. Geb.applicationLeft)
    rTy <- inferObject (app ^. Geb.applicationRight)
    case lTy of
      Geb.ObjectHom h -> do
        unless
          (h ^. Geb.homDomain == rTy)
          (error "Type mismatch: domain of the function and the argument")
        return $ h ^. Geb.homCodomain
      _ -> error "Left side of the application should be a function"
  Geb.MorphismBinop op -> do
    aTy <- inferObject (op ^. Geb.binopLeft)
    bTy <- inferObject (op ^. Geb.binopRight)
    unless (aTy == bTy) (error "Binop arguments should have the same type")
    return aTy
  Geb.MorphismLeft {} -> error $ lackOfInformation <> " on a left morphism"
  Geb.MorphismRight {} -> error $ lackOfInformation <> " on a right morphism"
  Geb.MorphismVar {} -> error $ lackOfInformation <> " on a variable"

lackOfInformation :: Text
lackOfInformation = "Not enough information to infer the type"