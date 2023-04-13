module Juvix.Compiler.Core.Translation.FromInternal.Builtins.Int where

import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

-- | Returns the node representing a function Int -> Int that transforms literal
-- integers to builtin Int.
literalIntToIntNode :: Member InfoTableBuilder r => Sem r Node
literalIntToIntNode = do
  tab <- getInfoTable
  let intToNatSymM = tab ^. infoLiteralIntToNat
      tagOfNatM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinIntOfNat
      tagNegSucM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinIntNegSuc
      boolSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinBool
  return $
    case (tagOfNatM, tagNegSucM, boolSymM, intToNatSymM) of
      (Just tagOfNat, Just tagNegSuc, Just boolSym, Just intToNatSym) ->
        mkLambda' mkTypeInteger' $
          mkIf'
            boolSym
            (mkBuiltinApp' OpIntLt [mkVar' 0, mkConstant' (ConstInteger 0)])
            ( mkConstr
                (setInfoName "negSuc" mempty)
                tagNegSuc
                [ mkBuiltinApp'
                    OpIntSub
                    [ mkConstant' (ConstInteger 0),
                      mkBuiltinApp' OpIntAdd [mkVar' 0, mkConstant' (ConstInteger 1)]
                    ]
                ]
            )
            ( mkConstr
                (setInfoName "ofNat" mempty)
                tagOfNat
                [mkApp' (mkIdent' intToNatSym) (mkVar' 0)]
            )
      _ -> mkLambda' mkTypeInteger' $ mkVar' 0
