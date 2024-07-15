module Juvix.Compiler.Core.Translation.FromInternal.Builtins.Nat where

import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo

-- | Returns the node representing a function Int -> Nat that is used to transform
-- literal integers to builtin Nat. The symbol representing the literalIntToNat function is passed
-- so that it can be called recursively.
literalIntToNatNode :: (Member InfoTableBuilder r) => Symbol -> Sem r Node
literalIntToNatNode sym = do
  md <- getModule
  let tagZeroM = (^. constructorTag) <$> lookupBuiltinConstructor md BuiltinNatZero
      tagSucM = (^. constructorTag) <$> lookupBuiltinConstructor md BuiltinNatSuc
      boolSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive md BuiltinBool
  return $ case (tagZeroM, tagSucM, boolSymM) of
    (Just tagZero, Just tagSuc, Just boolSym) ->
      mkLambda' mkTypeInteger'
        $ mkIf'
          boolSym
          (mkBuiltinApp' OpEq [mkVar' 0, mkConstant' (ConstInteger 0)])
          (mkConstr (setInfoName "zero" mempty) tagZero [])
          (mkConstr (setInfoName "suc" mempty) tagSuc [mkApp' (mkIdent' sym) (mkBuiltinApp' OpIntSub [mkVar' 0, mkConstant' (ConstInteger 1)])])
    _ -> mkLambda' mkTypeInteger' $ mkVar' 0
