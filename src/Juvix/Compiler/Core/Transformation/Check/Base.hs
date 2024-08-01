module Juvix.Compiler.Core.Transformation.Check.Base where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Data.Module
import Juvix.Compiler.Core.Data.TypeDependencyInfo (createTypeDependencyInfo)
import Juvix.Compiler.Core.Error
import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Info.LocationInfo (getInfoLocation, getNodeLocation)
import Juvix.Compiler.Core.Info.TypeInfo qualified as Info
import Juvix.Compiler.Core.Transformation.Base (mapT')
import Juvix.Data.NameKind
import Juvix.Data.PPOutput

dynamicTypeError :: Node -> Maybe Location -> CoreError
dynamicTypeError node loc =
  CoreError
    { _coreErrorMsg = ppOutput "compilation for this target requires full type information",
      _coreErrorNode = Just node,
      _coreErrorLoc = fromMaybe defaultLoc loc
    }

axiomError :: (Members '[Error CoreError, InfoTableBuilder] r) => Symbol -> Maybe Location -> Sem r a
axiomError sym loc = do
  md <- getModule
  let nameTxt = identName md sym
  throw
    CoreError
      { _coreErrorMsg = ppOutput ("The symbol" <+> annotate (AnnKind KNameAxiom) (pretty nameTxt) <> " is defined as an axiom and thus it cannot be compiled"),
        _coreErrorNode = Nothing,
        _coreErrorLoc = fromMaybe defaultLoc loc
      }

unsupportedError :: Text -> Node -> Maybe Location -> CoreError
unsupportedError what node loc =
  CoreError
    { _coreErrorMsg = ppOutput $ pretty what <> " not supported for this target",
      _coreErrorNode = Just node,
      _coreErrorLoc = fromMaybe defaultLoc loc
    }

defaultLoc :: Interval
defaultLoc = singletonInterval (mkInitialLoc mockFile)
  where
    mockFile :: Path Abs File
    mockFile = $(mkAbsFile "/core-check")

checkBuiltins :: forall r. (Member (Error CoreError) r) => Bool -> Node -> Sem r Node
checkBuiltins allowUntypedFail = dmapRM go
  where
    go :: Node -> Sem r Recur
    go node = case node of
      NPrim TypePrim {..}
        | _typePrimPrimitive == PrimString ->
            throw $ unsupportedError "strings" node (getInfoLocation _typePrimInfo)
      NBlt BuiltinApp {..} ->
        case _builtinAppOp of
          OpTrace -> throw $ unsupportedError "tracing" node (getInfoLocation _builtinAppInfo)
          OpFail | not allowUntypedFail -> do
            let ty = Info.getInfoType _builtinAppInfo
            when (isDynamic ty) $
              throw $
                unsupportedError "failing without type info" node (getInfoLocation _builtinAppInfo)
            return $ Recur node
          OpFail -> do
            return $ End node
          _
            | _builtinAppOp `elem` builtinsString ->
                throw $ unsupportedError "strings" node (getInfoLocation _builtinAppInfo)
            | _builtinAppOp `elem` builtinsCairo ->
                throw $ unsupportedError "cairo" node (getInfoLocation _builtinAppInfo)
            | _builtinAppOp `elem` builtinsAnoma ->
                throw $ unsupportedError "anoma" node (getInfoLocation _builtinAppInfo)
            | otherwise ->
                return $ Recur node
      _ -> return $ Recur node

checkBuiltins' :: forall r. (Member (Error CoreError) r) => [BuiltinOp] -> [Primitive] -> Node -> Sem r Node
checkBuiltins' unsupportedOps unsupportedTypes = dmapRM go
  where
    go :: Node -> Sem r Recur
    go node = case node of
      NPrim TypePrim {..}
        | _typePrimPrimitive `elem` unsupportedTypes ->
            throw $ unsupportedError "type" node (getInfoLocation _typePrimInfo)
      NBlt BuiltinApp {..}
        | _builtinAppOp `elem` unsupportedOps ->
            throw $ unsupportedError "operation" node (getInfoLocation _builtinAppInfo)
        | otherwise -> case _builtinAppOp of
            OpFail -> return $ End node
            _ -> return $ Recur node
      _ -> return $ Recur node

-- | Checks that the root of the node is not `Bottom`. Currently the only way we
-- create `Bottom` is when translating axioms that are not builtin. Hence it is
-- enough to check the root only.
checkNoAxioms :: forall r. (Member (Error CoreError) r) => Module -> Sem r ()
checkNoAxioms = void . mapT' checkNodeNoAxiom
  where
    checkNodeNoAxiom :: Symbol -> Node -> Sem (InfoTableBuilder ': r) Node
    checkNodeNoAxiom sym n = case n of
      NBot {} -> axiomError sym (getNodeLocation n)
      _ -> return n

checkNoIO :: forall r. (Member (Error CoreError) r) => Node -> Sem r Node
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

checkTypes :: forall r. (Member (Error CoreError) r) => Bool -> Module -> Node -> Sem r Node
checkTypes allowPolymorphism md = dmapM go
  where
    go :: Node -> Sem r Node
    go node = case node of
      NIdt Ident {..}
        | isDynamic (lookupIdentifierInfo md _identSymbol ^. identifierType) ->
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
        | not allowPolymorphism && isTypeConstr md (_piBinder ^. binderType) ->
            throw
              CoreError
                { _coreErrorMsg = ppOutput "polymorphism not supported for this target",
                  _coreErrorNode = Just node,
                  _coreErrorLoc = fromMaybe defaultLoc (_piBinder ^. binderLocation)
                }
      _ -> return node

checkNoRecursiveTypes :: forall r. (Member (Error CoreError) r) => Module -> Sem r ()
checkNoRecursiveTypes md =
  when (isCyclic (createTypeDependencyInfo (md ^. moduleInfoTable))) $
    throw
      CoreError
        { _coreErrorMsg = ppOutput "recursive types not supported for this target",
          _coreErrorNode = Nothing,
          _coreErrorLoc = defaultLoc
        }

checkMainExists :: forall r. (Member (Error CoreError) r) => Module -> Sem r ()
checkMainExists md =
  when (isNothing (md ^. moduleInfoTable . infoMain)) $
    throw
      CoreError
        { _coreErrorMsg = ppOutput "no `main` function",
          _coreErrorNode = Nothing,
          _coreErrorLoc = defaultLoc
        }

checkMainTypeExec :: (Member (Error CoreError) r) => Module -> Sem r ()
checkMainTypeExec md =
  case md ^. moduleInfoTable . infoMain of
    Nothing ->
      throw
        CoreError
          { _coreErrorMsg = ppOutput "no `main` function",
            _coreErrorNode = Nothing,
            _coreErrorLoc = defaultLoc
          }
    Just sym ->
      case ii ^. identifierType of
        NPi {} ->
          throw
            CoreError
              { _coreErrorMsg = ppOutput "`main` cannot have a function type for this target",
                _coreErrorNode = Nothing,
                _coreErrorLoc = loc
              }
        ty
          | isTypeConstr md ty ->
              throw
                CoreError
                  { _coreErrorMsg = ppOutput "`main` cannot be a type for this target",
                    _coreErrorNode = Nothing,
                    _coreErrorLoc = loc
                  }
        _ ->
          return ()
      where
        ii = lookupIdentifierInfo md sym
        loc = fromMaybe defaultLoc (ii ^. identifierLocation)
