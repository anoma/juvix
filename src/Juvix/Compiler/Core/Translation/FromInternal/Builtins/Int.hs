module Juvix.Compiler.Core.Translation.FromInternal.Builtins.Int where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

setupLiteralIntToInt :: Symbol -> InfoTable -> InfoTable
setupLiteralIntToInt sym tab =
  tab
    { _infoIdentifiers = HashMap.insert sym ii (tab ^. infoIdentifiers),
      _identContext = HashMap.insert sym node (tab ^. identContext),
      _infoLiteralIntToInt = Just sym
    }
  where
    ii =
      IdentifierInfo
        { _identifierSymbol = sym,
          _identifierName = freshIdentName tab "intToNat",
          _identifierLocation = Nothing,
          _identifierArgsNum = 1,
          _identifierType = mkPi mempty (Binder "x" Nothing mkTypeInteger') targetType,
          _identifierIsExported = False,
          _identifierBuiltin = Nothing
        }
    node =
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

    targetType = maybe mkTypeInteger' (\s -> mkTypeConstr (setInfoName "Int" mempty) s []) natIntM
    tagOfNatM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinIntOfNat
    tagNegSucM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinIntNegSuc
    boolSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinBool
    natIntM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinInt
    intToNatSymM = tab ^. infoLiteralIntToNat
