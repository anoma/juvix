module Juvix.Compiler.Core.Translation.Stripped.FromCore (fromCore) where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core
import Juvix.Compiler.Core.Data.Stripped.InfoTable qualified as Stripped
import Juvix.Compiler.Core.Extra.Stripped.Base qualified as Stripped
import Juvix.Compiler.Core.Info.LocationInfo
import Juvix.Compiler.Core.Info.NameInfo
import Juvix.Compiler.Core.Language.Stripped qualified as Stripped
import Juvix.Compiler.Core.Pretty

fromCore :: InfoTable -> Stripped.InfoTable
fromCore tab =
  Stripped.InfoTable
    { _infoMain = tab ^. infoMain,
      _infoFunctions = fmap (translateFunctionInfo tab) (tab' ^. infoIdentifiers),
      _infoInductives = fmap translateInductiveInfo (tab' ^. infoInductives),
      _infoConstructors = fmap translateConstructorInfo (tab' ^. infoConstructors)
    }
  where
    tab' =
      tab
        { _infoIdentifiers = HashMap.filter (\ii -> isNothing (ii ^. identifierBuiltin)) (tab ^. infoIdentifiers),
          _infoInductives = HashMap.filter (\ii -> isNothing (ii ^. inductiveBuiltin) || isIO (ii ^. inductiveBuiltin)) (tab ^. infoInductives),
          _infoConstructors = HashMap.filter (\ci -> isNothing (ci ^. constructorBuiltin)) (tab ^. infoConstructors)
        }
    isIO :: Maybe BuiltinType -> Bool
    isIO (Just (BuiltinTypeAxiom BuiltinIO)) = True
    isIO _ = False

translateFunctionInfo :: InfoTable -> IdentifierInfo -> Stripped.FunctionInfo
translateFunctionInfo tab IdentifierInfo {..} =
  Stripped.FunctionInfo
    { _functionName = _identifierName,
      _functionLocation = _identifierLocation,
      _functionSymbol = _identifierSymbol,
      _functionBody =
        translateFunction
          _identifierArgsNum
          (fromJust $ HashMap.lookup _identifierSymbol (tab ^. identContext)),
      _functionType = translateType _identifierType,
      _functionArgsNum = _identifierArgsNum,
      _functionArgsInfo = map translateArgInfo _identifierArgsInfo,
      _functionIsExported = _identifierIsExported
    }

translateArgInfo :: ArgumentInfo -> Stripped.ArgumentInfo
translateArgInfo ArgumentInfo {..} =
  Stripped.ArgumentInfo
    { _argumentName = _argumentName,
      _argumentLocation = _argumentLocation,
      _argumentType = translateType _argumentType
    }

translateInductiveInfo :: InductiveInfo -> Stripped.InductiveInfo
translateInductiveInfo InductiveInfo {..} =
  Stripped.InductiveInfo
    { _inductiveName = _inductiveName,
      _inductiveLocation = _inductiveLocation,
      _inductiveSymbol = _inductiveSymbol,
      _inductiveKind = translateType _inductiveKind,
      _inductiveConstructors = map translateConstructorInfo _inductiveConstructors,
      _inductiveParams = map translateParamInfo _inductiveParams
    }

translateParamInfo :: ParameterInfo -> Stripped.ParameterInfo
translateParamInfo ParameterInfo {..} =
  Stripped.ParameterInfo
    { _paramName = _paramName,
      _paramLocation = _paramLocation,
      _paramKind = translateType _paramKind,
      _paramIsImplicit = _paramIsImplicit
    }

translateConstructorInfo :: ConstructorInfo -> Stripped.ConstructorInfo
translateConstructorInfo ConstructorInfo {..} =
  Stripped.ConstructorInfo
    { _constructorName = _constructorName,
      _constructorLocation = _constructorLocation,
      _constructorInductive = _constructorInductive,
      _constructorTag = _constructorTag,
      _constructorType = translateType _constructorType
    }

translateFunction :: Int -> Node -> Stripped.Node
translateFunction argsNum node =
  let (k, body) = unfoldLambdas' node
   in if
          | k /= argsNum ->
              error
                ( "wrong number of arguments. argsNum = "
                    <> show argsNum
                    <> ", unfoldLambdas = "
                    <> show k
                    <> "\nNode = "
                    <> ppTrace node
                )
          | otherwise -> translateNode body

translateNode :: Node -> Stripped.Node
translateNode node = case node of
  NVar v ->
    Stripped.NVar $ translateVar v
  NIdt idt ->
    Stripped.NIdt $ translateIdent idt
  NCst Constant {..} ->
    Stripped.mkConstant _constantValue
  NApp App {} ->
    let (tgt, args) = unfoldApps' node
        args' = map translateNode args
     in case tgt of
          NVar v -> Stripped.mkApps (Stripped.FunVar $ translateVar v) args'
          NIdt idt -> Stripped.mkApps (Stripped.FunIdent $ translateIdent idt) args'
          _ -> unsupported
  NBlt BuiltinApp {..} ->
    Stripped.mkBuiltinApp _builtinAppOp (map translateNode _builtinAppArgs)
  NCtr Constr {..} ->
    Stripped.mkConstr
      ( Stripped.ConstrInfo
          { _constrInfoName = getInfoName _constrInfo,
            _constrInfoLocation = getInfoLocation _constrInfo,
            _constrInfoType = Stripped.TyDynamic
          }
      )
      _constrTag
      (map translateNode _constrArgs)
  NLet Let {..} ->
    Stripped.mkLet
      ( Stripped.Binder
          { _binderName = _letItem ^. letItemBinder . binderName,
            _binderLocation = _letItem ^. letItemBinder . binderLocation,
            _binderType = translateType (_letItem ^. letItemBinder . binderType)
          }
      )
      (translateNode (_letItem ^. letItemValue))
      (translateNode _letBody)
  NCase Case {..} -> case _caseBranches of
    [br@CaseBranch {..}]
      | _caseBranchTag == BuiltinTag TagTrue ->
          translateIf _caseValue (br ^. caseBranchBody) (fromMaybe branchFailure _caseDefault)
    [br@CaseBranch {..}]
      | _caseBranchTag == BuiltinTag TagFalse ->
          translateIf _caseValue (fromMaybe branchFailure _caseDefault) (br ^. caseBranchBody)
    [br1, br2]
      | br1 ^. caseBranchTag == BuiltinTag TagTrue
          && br2 ^. caseBranchTag == BuiltinTag TagFalse ->
          translateIf _caseValue (br1 ^. caseBranchBody) (br2 ^. caseBranchBody)
      | br1 ^. caseBranchTag == BuiltinTag TagFalse
          && br2 ^. caseBranchTag == BuiltinTag TagTrue ->
          translateIf _caseValue (br2 ^. caseBranchBody) (br1 ^. caseBranchBody)
    _ ->
      Stripped.mkCase
        _caseInductive
        (translateNode _caseValue)
        (map translateCaseBranch _caseBranches)
        (fmap translateNode _caseDefault)
  _
    | isType node ->
        Stripped.mkConstr (Stripped.ConstrInfo "()" Nothing Stripped.TyDynamic) (BuiltinTag TagTrue) []
  _ ->
    unsupported
  where
    unsupported :: a
    unsupported = error "Core to Core.Stripped: unsupported node"

    translateIf :: Node -> Node -> Node -> Stripped.Node
    translateIf val br1 br2 = Stripped.mkIf (translateNode val) (translateNode br1) (translateNode br2)

    branchFailure :: Node
    branchFailure = mkBuiltinApp' OpFail [mkConstant' (ConstString "illegal `if` branch")]

    translateCaseBranch :: CaseBranch -> Stripped.CaseBranch
    translateCaseBranch CaseBranch {..}
      | _caseBranchTag == BuiltinTag TagTrue || _caseBranchTag == BuiltinTag TagFalse =
          error "Core to Core.Stripped: invalid case on booleans"
    translateCaseBranch CaseBranch {..} =
      Stripped.CaseBranch
        { _caseBranchInfo =
            Stripped.CaseBranchInfo
              { _caseBranchInfoConstrName = getInfoName _caseBranchInfo,
                _caseBranchInfoConstrType = Stripped.TyDynamic
              },
          _caseBranchTag = _caseBranchTag,
          _caseBranchBinders = map (over binderType translateType) _caseBranchBinders,
          _caseBranchBindersNum = _caseBranchBindersNum,
          _caseBranchBody = translateNode _caseBranchBody
        }

    translateVar :: Var -> Stripped.Var
    translateVar Var {..} =
      Stripped.Var
        ( Stripped.VarInfo
            { _varInfoName = getInfoName _varInfo,
              _varInfoLocation = getInfoLocation _varInfo,
              _varInfoType = Stripped.TyDynamic
            }
        )
        _varIndex

    translateIdent :: Ident -> Stripped.Ident
    translateIdent Ident {..} =
      Stripped.Ident
        ( Stripped.IdentInfo
            { _identInfoName = getInfoName _identInfo,
              _identInfoLocation = getInfoLocation _identInfo,
              _identInfoType = Stripped.TyDynamic
            }
        )
        _identSymbol

translateType :: Node -> Stripped.Type
translateType node = case node of
  NVar {} ->
    Stripped.TyDynamic
  NPi Pi {} ->
    let (args, tgt) = unfoldPi node
        tyargs = map (^. piLhsBinder . binderType) args
     in Stripped.mkFunType (map translateType tyargs) (translateType tgt)
  NUniv Univ {} ->
    Stripped.TyDynamic
  NTyp TypeConstr {..} ->
    Stripped.TyApp $
      Stripped.TypeApp
        { _typeAppName = getInfoName _typeConstrInfo,
          _typeAppLocation = getInfoLocation _typeConstrInfo,
          _typeAppSymbol = _typeConstrSymbol,
          _typeAppArgs = map translateType _typeConstrArgs
        }
  NPrim TypePrim {..} ->
    Stripped.TyPrim _typePrimPrimitive
  NDyn Dynamic {} ->
    Stripped.TyDynamic
  _ ->
    Stripped.TyDynamic

-- TODO: We need to return TyDynamic here to handle type synonyms. This should
-- be handled by RemoveTypeArgs, but currently it cannot be because
-- lambda-letrec-lifting doesn't preserve the type information about the body.

-- _ -> error $ "Core to Core.Stripped: unsupported type: " <> ppTrace node
