module Juvix.Compiler.Core.Transformation.Check.Cairo where

import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Pretty
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Compiler.Core.Transformation.Check.Base

checkCairo :: forall r. (Member (Error CoreError) r) => Module -> Sem r Module
checkCairo md = do
  checkMainExists md
  checkMainType
  checkNoAxioms md
  mapAllNodesM checkNoIO md
  mapAllNodesM (checkBuiltins' (builtinsString ++ builtinsUInt8 ++ builtinsByteArray) [PrimString, primitiveUInt8, PrimByteArray]) md
  where
    checkMainType :: Sem r ()
    checkMainType =
      unless (checkType (ii ^. identifierType)) $
        throw
          CoreError
            { _coreErrorMsg = ppOutput "for this target the arguments the `main` function need to be field elements, numbers, booleans, records or lists, and the result needs to be a field element, number, boolean or a record of field elements, numbers and booleans",
              _coreErrorLoc = fromMaybe defaultLoc (ii ^. identifierLocation),
              _coreErrorNode = Nothing
            }
      where
        ii = lookupIdentifierInfo md (fromJust (getInfoMain md))

    checkType :: Node -> Bool
    checkType ty =
      let (tyargs, tgt) = unfoldPi' ty
       in all isArgType tyargs && isTargetType tgt
      where
        isArgType :: Node -> Bool
        isArgType = \case
          NPi {} -> False
          NUniv {} -> False
          NTyp x -> isRecordOrList x
          NPrim x -> isAllowedPrim x
          NDyn {} -> True
          _ -> False

        isTargetType :: Node -> Bool
        isTargetType = \case
          NPi {} -> False
          NUniv {} -> False
          NTyp x -> isFlatRecord x
          NPrim x -> isAllowedPrim x
          NDyn {} -> True
          _ -> False

        isPrimType :: Node -> Bool
        isPrimType = \case
          NPrim x -> isAllowedPrim x
          _ -> False

        isAllowedPrim :: TypePrim -> Bool
        isAllowedPrim TypePrim {..} = case _typePrimPrimitive of
          PrimInteger {} -> True
          PrimBool {} -> True
          PrimField {} -> True
          PrimString {} -> False
          PrimByteArray {} -> False
          PrimRandomGenerator {} -> False

        isRecordOrList :: TypeConstr -> Bool
        isRecordOrList TypeConstr {..} = case ii ^. inductiveBuiltin of
          Just (BuiltinTypeInductive BuiltinList) ->
            all isArgType _typeConstrArgs
          Just (BuiltinTypeInductive BuiltinPair) ->
            all isArgType _typeConstrArgs
          Just {} ->
            False
          Nothing ->
            case ii ^. inductiveConstructors of
              [tag] ->
                all isArgType tyargs
                where
                  ci = lookupConstructorInfo md tag
                  cty = ci ^. constructorType
                  nParams = length _typeConstrArgs
                  tyargs =
                    map (substs _typeConstrArgs)
                      . drop nParams
                      . typeArgs
                      $ cty
              _ -> False
          where
            ii = lookupInductiveInfo md _typeConstrSymbol

        isFlatRecord :: TypeConstr -> Bool
        isFlatRecord TypeConstr {..} =
          case ii ^. inductiveConstructors of
            [tag]
              | null _typeConstrArgs ->
                  all isPrimType (typeArgs (ci ^. constructorType))
              where
                ci = lookupConstructorInfo md tag
            _ -> False
          where
            ii = lookupInductiveInfo md _typeConstrSymbol
