module Juvix.Compiler.Core.Transformation.CheckGeb where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.IdentDependencyInfo
import Juvix.Compiler.Core.Data.TypeDependencyInfo
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo (getInfoLocation)
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base
import Juvix.Data.PPOutput

checkGeb :: forall r. Member (Error CoreError) r => InfoTable -> Sem r InfoTable
checkGeb tab =
  checkNoRecursion
    >> checkNoRecursiveTypes
    >> mapAllNodesM checkNoIO tab
    >> mapAllNodesM checkBuiltins tab
    >> mapAllNodesM checkTypes tab
  where
    checkTypes :: Node -> Sem r Node
    checkTypes = dmapM go
      where
        go :: Node -> Sem r Node
        go node = case node of
          NIdt Ident {..}
            | isDynamic (fromJust (HashMap.lookup _identSymbol (tab ^. infoIdentifiers)) ^. identifierType) ->
                throw (dynamicTypeError node (getInfoLocation _identInfo))
          NLam Lambda {..}
            | isDynamic (_lambdaBinder ^. binderType) ->
                throw (dynamicTypeError node (_lambdaBinder ^. binderLocation))
          NLet Let {..}
            | isDynamic (_letItem ^. letItemBinder . binderType) ->
                throw (dynamicTypeError node (_letItem ^. letItemBinder . binderLocation))
          NRec LetRec {..}
            | any (isDynamic . (^. letItemBinder . binderType)) _letRecValues ->
                throw (dynamicTypeError node (head _letRecValues ^. letItemBinder . binderLocation))
          NPi Pi {..}
            | isTypeConstr tab (_piBinder ^. binderType) ->
                throw
                  CoreError
                    { _coreErrorMsg = ppOutput "polymorphism not supported for the GEB target",
                      _coreErrorNode = Just node,
                      _coreErrorLoc = fromMaybe defaultLoc (_piBinder ^. binderLocation)
                    }
          _ -> return node

    checkBuiltins :: Node -> Sem r Node
    checkBuiltins = dmapM go
      where
        go :: Node -> Sem r Node
        go node = case node of
          NPrim TypePrim {..}
            | _typePrimPrimitive == PrimString ->
                throw $ unsupportedError "strings" node (getInfoLocation _typePrimInfo)
          NBlt BuiltinApp {..} ->
            case _builtinAppOp of
              OpShow -> throw $ unsupportedError "strings" node (getInfoLocation _builtinAppInfo)
              OpStrConcat -> throw $ unsupportedError "strings" node (getInfoLocation _builtinAppInfo)
              OpStrToInt -> throw $ unsupportedError "strings" node (getInfoLocation _builtinAppInfo)
              OpTrace -> throw $ unsupportedError "tracing" node (getInfoLocation _builtinAppInfo)
              OpFail -> do
                let ty = Info.getInfoType _builtinAppInfo
                when (isDynamic ty) $
                  throw $
                    unsupportedError "failing without type info" node (getInfoLocation _builtinAppInfo)
                return node
              _ -> return node
          _ -> return node

    checkNoIO :: Node -> Sem r Node
    checkNoIO = dmapM go
      where
        go :: Node -> Sem r Node
        go node = case node of
          NCtr Constr {..} ->
            case _constrTag of
              BuiltinTag TagReturn -> throw $ unsupportedError "IO" node (getInfoLocation _constrInfo)
              BuiltinTag TagBind -> throw $ unsupportedError "IO" node (getInfoLocation _constrInfo)
              BuiltinTag TagReadLn -> throw $ unsupportedError "IO" node (getInfoLocation _constrInfo)
              BuiltinTag TagWrite -> throw $ unsupportedError "IO" node (getInfoLocation _constrInfo)
              _ -> return node
          _ -> return node

    checkNoRecursion :: Sem r ()
    checkNoRecursion =
      when (isCyclic (createIdentDependencyInfo tab)) $
        throw
          CoreError
            { _coreErrorMsg = ppOutput "recursion not supported for the GEB target",
              _coreErrorNode = Nothing,
              _coreErrorLoc = defaultLoc
            }

    checkNoRecursiveTypes :: Sem r ()
    checkNoRecursiveTypes =
      when (isCyclic (createTypeDependencyInfo tab)) $
        throw
          CoreError
            { _coreErrorMsg = ppOutput "recursive types not supported for the GEB target",
              _coreErrorNode = Nothing,
              _coreErrorLoc = defaultLoc
            }

    dynamicTypeError :: Node -> Maybe Location -> CoreError
    dynamicTypeError node loc =
      CoreError
        { _coreErrorMsg = ppOutput "compilation for the GEB target requires full type information",
          _coreErrorNode = Just node,
          _coreErrorLoc = fromMaybe defaultLoc loc
        }

    unsupportedError :: Text -> Node -> Maybe Location -> CoreError
    unsupportedError what node loc =
      CoreError
        { _coreErrorMsg = ppOutput $ pretty what <> " not supported for the GEB target",
          _coreErrorNode = Just node,
          _coreErrorLoc = fromMaybe defaultLoc loc
        }

    mockFile :: Path Abs File
    mockFile = $(mkAbsFile "/core-to-geb")

    defaultLoc :: Interval
    defaultLoc = singletonInterval (mkInitialLoc mockFile)
