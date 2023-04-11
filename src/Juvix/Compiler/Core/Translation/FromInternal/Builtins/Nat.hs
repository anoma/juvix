module Juvix.Compiler.Core.Translation.FromInternal.Builtins.Nat where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language

setupIntToNat :: Symbol -> InfoTable -> InfoTable
setupIntToNat sym tab =
  tab
    { _infoIdentifiers = HashMap.insert sym ii (tab ^. infoIdentifiers),
      _identContext = HashMap.insert sym node (tab ^. identContext),
      _infoLiteralIntToNat = Just sym
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
      case (tagZeroM, tagSucM, boolSymM) of
        (Just tagZero, Just tagSuc, Just boolSym) ->
          mkLambda' mkTypeInteger' $
            mkIf'
              boolSym
              (mkBuiltinApp' OpEq [mkVar' 0, mkConstant' (ConstInteger 0)])
              (mkConstr (setInfoName "zero" mempty) tagZero [])
              (mkConstr (setInfoName "suc" mempty) tagSuc [mkApp' (mkIdent' sym) (mkBuiltinApp' OpIntSub [mkVar' 0, mkConstant' (ConstInteger 1)])])
        _ ->
          mkLambda' mkTypeInteger' $ mkVar' 0
    targetType = maybe mkTypeInteger' (\s -> mkTypeConstr (setInfoName "Nat" mempty) s []) natSymM
    tagZeroM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinNatZero
    tagSucM = (^. constructorTag) <$> lookupBuiltinConstructor tab BuiltinNatSuc
    boolSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinBool
    natSymM = (^. inductiveSymbol) <$> lookupBuiltinInductive tab BuiltinNat
