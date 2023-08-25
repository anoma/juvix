module Juvix.Compiler.Core.Translation.FromInternal.Builtins.Nat where

import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

-- | Returns the node representing a function Int -> Nat that is used to transform
-- literal integers to builtin Nat. The symbol representing the literalIntToNat function is passed
-- so that it can be called recusively.
literalIntToNatNode :: (Member InfoTableBuilder r) => Symbol -> Sem r Node
literalIntToNatNode sym = do
  tab <- getInfoTable
  let tagZeroM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinNatZero
      tagSucM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinNatSuc
      boolSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinBool
  return $ case (tagZeroM, tagSucM, boolSymM) of
    (Just tagZero, Just tagSuc, Just boolSym) ->
      mkLambda' mkTypeInteger' $
        mkIf'
          boolSym
          (mkBuiltinApp' OpEq [mkVar' 0, mkConstant' (ConstInteger 0)])
          (mkConstr (setInfoName "zero" mempty) tagZero [])
          (mkConstr (setInfoName "suc" mempty) tagSuc [mkApp' (mkIdent' sym) (mkBuiltinApp' OpIntSub [mkVar' 0, mkConstant' (ConstInteger 1)])])
    _ -> mkLambda' mkTypeInteger' $ mkVar' 0
