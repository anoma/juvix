module Juvix.Compiler.Backend.C.Data.Base
  ( module Juvix.Compiler.Backend.C.Data.Base,
    module Juvix.Compiler.Backend.C.Data.Types,
    module Juvix.Compiler.Backend.C.Data.CNames,
    module Juvix.Compiler.Backend.C.Data.CBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Juvix.Compiler.Backend.C.Data.BuiltinTable
import Juvix.Compiler.Backend.C.Data.CBuilder
import Juvix.Compiler.Backend.C.Data.CNames
import Juvix.Compiler.Backend.C.Data.Types
import Juvix.Compiler.Backend.C.Language
import Juvix.Compiler.Internal.Extra (mkPolyType')
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

unsupported :: Text -> a
unsupported msg = error (msg <> " Internal to C: not yet supported")

-- The direct use of Internal.PolyType is safe here
unfoldFunType :: Internal.PolyType -> ([Internal.PolyType], Internal.PolyType)
unfoldFunType t = (map (Internal.PolyType . (^. Internal.paramType)) params, Internal.PolyType ret)
  where
    (params, ret) = Internal.unfoldFunType (t ^. Internal.unpolyType)

unfoldPolyApp :: Member (Reader Internal.TypesTable) r => Internal.Application -> Sem r (Internal.Expression, [Internal.Expression])
unfoldPolyApp a =
  let (f, args) = Internal.unfoldApplication a
   in case f of
        Internal.ExpressionLiteral {} -> return (f, toList args)
        Internal.ExpressionIden iden -> do
          args' <- filterCompileTimeArgsOrPatterns (Internal.getName iden) (toList args)
          return (f, args')
        _ -> impossible

filterCompileTimeArgsOrPatterns :: Member (Reader Internal.TypesTable) r => Internal.Name -> [a] -> Sem r [a]
filterCompileTimeArgsOrPatterns idenName lst = do
  tab <- ask
  return $
    map fst $
      filter (not . isUniverse . snd) $
        zip lst (map (^. Internal.paramType) (fst (Internal.unfoldFunType (ty tab))))
  where
    ty = HashMap.lookupDefault impossible idenName
    isUniverse :: Internal.Expression -> Bool
    isUniverse = \case
      (Internal.ExpressionUniverse {}) -> True
      _ -> False

mkName :: Internal.Name -> Text
mkName n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | T.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = T.filter isValidChar (n ^. Internal.nameText)
    isValidChar :: Char -> Bool
    isValidChar c = isLetter c && isAscii c
    adaptFirstLetter :: Text -> Text
    adaptFirstLetter t = case T.uncons t of
      Nothing -> impossible
      Just (h, r) -> T.cons (capitalize h) r
      where
        capitalize :: Char -> Char
        capitalize
          | capital = toUpper
          | otherwise = toLower
        capital = case n ^. Internal.nameKind of
          Internal.KNameConstructor -> True
          Internal.KNameInductive -> True
          Internal.KNameTopModule -> True
          Internal.KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Internal.nameKind of
      Internal.KNameTopModule -> mempty
      Internal.KNameFunction ->
        if n ^. Internal.nameText == Str.main then mempty else idSuffix
      _ -> idSuffix
    idSuffix :: Text
    idSuffix = "_" <> show (n ^. Internal.nameId . Internal.unNameId)

goType :: forall r. Member (Reader Internal.InfoTable) r => Internal.PolyType -> Sem r CDeclType
goType t = case t ^. Internal.unpolyType of
  Internal.ExpressionIden ti -> getInternalType ti
  Internal.ExpressionFunction {} -> return declFunctionPtrType
  Internal.ExpressionUniverse {} -> unsupported "TypeUniverse"
  Internal.ExpressionApplication a -> goType (mkPolyType' (fst (Internal.unfoldApplication a)))
  Internal.ExpressionLiteral {} -> impossible
  Internal.ExpressionHole {} -> impossible
  Internal.ExpressionSimpleLambda {} -> impossible
  Internal.ExpressionLambda {} -> impossible
  where
    getInternalType :: Internal.Iden -> Sem r CDeclType
    getInternalType = \case
      Internal.IdenInductive mn -> getInductiveCType mn
      Internal.IdenAxiom mn -> do
        axiomName <- getAxiomCName mn
        return
          CDeclType
            { _typeDeclType = DeclTypeDefType axiomName,
              _typeIsPtr = False
            }
      Internal.IdenVar {} ->
        return
          CDeclType
            { _typeDeclType = uIntPtrType,
              _typeIsPtr = False
            }
      _ -> impossible

typeToFunType :: Member (Reader Internal.InfoTable) r => Internal.PolyType -> Sem r CFunType
typeToFunType t = do
  let (args, ret) = unfoldFunType t
  _cFunArgTypes <- mapM goType args
  _cFunReturnType <- goType ret
  return CFunType {..}

applyOnFunStatement ::
  forall a. Monoid a => (Internal.FunctionDef -> a) -> Internal.Statement -> a
applyOnFunStatement f = \case
  Internal.StatementFunction (Internal.MutualBlock x) -> mconcatMap f x
  Internal.StatementForeign {} -> mempty
  Internal.StatementAxiom {} -> mempty
  Internal.StatementInductive {} -> mempty
  Internal.StatementInclude i -> mconcat $ map (applyOnFunStatement f) (i ^. Internal.includeModule . Internal.moduleBody . Internal.moduleStatements)

getConstructorCName :: Members '[Reader Internal.InfoTable] r => Internal.Name -> Sem r Text
getConstructorCName n = do
  ctorInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoConstructors)
  return
    ( case ctorInfo ^. Internal.constructorInfoBuiltin of
        Just builtin -> fromJust (builtinConstructorName builtin)
        Nothing -> mkName n
    )

getAxiomCName :: Members '[Reader Internal.InfoTable] r => Internal.Name -> Sem r Text
getAxiomCName n = do
  axiomInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoAxioms)
  return
    ( case axiomInfo ^. Internal.axiomInfoBuiltin of
        Just builtin -> fromJust (builtinAxiomName builtin)
        Nothing -> mkName n
    )

getInductiveCName :: Members '[Reader Internal.InfoTable] r => Internal.Name -> Sem r (Bool, Text)
getInductiveCName n = do
  inductiveInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoInductives)
  return
    ( case inductiveInfo ^. (Internal.inductiveInfoDef . Internal.inductiveBuiltin) of
        Just builtin -> (False, fromJust (builtinInductiveName builtin))
        Nothing -> (True, asTypeDef (mkName n))
    )

getInductiveCType :: Member (Reader Internal.InfoTable) r => Internal.Name -> Sem r CDeclType
getInductiveCType n = do
  (isPtr, name) <- getInductiveCName n
  return
    ( CDeclType
        { _typeDeclType = DeclTypeDefType name,
          _typeIsPtr = isPtr
        }
    )

typeOfConstructor :: Member (Reader Internal.InfoTable) r => Internal.Name -> Sem r CDeclType
typeOfConstructor name = do
  info <- Internal.lookupConstructor name
  getInductiveCType (info ^. Internal.constructorInfoInductive)

getClausePatterns :: Member (Reader Internal.TypesTable) r => Internal.FunctionClause -> Sem r [Internal.Pattern]
getClausePatterns c =
  filterCompileTimeArgsOrPatterns
    (c ^. Internal.clauseName)
    ( c
        ^.. Internal.clausePatterns
          . each
          . Internal.patternArgPattern
    )

functionInfoPatternsNum :: Member (Reader Internal.TypesTable) r => Internal.FunctionInfo -> Sem r Int
functionInfoPatternsNum fInfo = do
  let c = head (fInfo ^. (Internal.functionInfoDef . Internal.funDefClauses))
  pats <- getClausePatterns c
  return (length pats)

buildPatternInfoTable :: forall r. Members '[Reader Internal.InfoTable, Reader Internal.TypesTable] r => [Internal.PolyType] -> Internal.FunctionClause -> Sem r PatternInfoTable
buildPatternInfoTable argTyps c =
  PatternInfoTable . HashMap.fromList <$> patBindings
  where
    funArgBindings :: Sem r [(Expression, CFunType)]
    funArgBindings = mapM (bimapM (return . ExpressionVar) typeToFunType) (zip funArgs argTyps)

    patArgBindings :: Sem r [(Internal.Pattern, (Expression, CFunType))]
    patArgBindings = do
      pats <- getClausePatterns c
      zip pats <$> funArgBindings

    patBindings :: Sem r [(Text, BindingInfo)]
    patBindings = patArgBindings >>= concatMapM go

    go :: (Internal.Pattern, (Expression, CFunType)) -> Sem r [(Text, BindingInfo)]
    go (p, (exp, typ)) = case p of
      Internal.PatternVariable v ->
        return
          [(v ^. Internal.nameText, BindingInfo {_bindingInfoExpr = exp, _bindingInfoType = typ})]
      Internal.PatternConstructorApp Internal.ConstructorApp {..} ->
        goConstructorApp exp _constrAppConstructor (_constrAppParameters ^.. each . Internal.patternArgPattern)
      Internal.PatternWildcard {} -> return []

    goConstructorApp :: Expression -> Internal.Name -> [Internal.Pattern] -> Sem r [(Text, BindingInfo)]
    goConstructorApp exp constructorName ps = do
      ctorInfo' <- ctorInfo
      let ctorArgBindings :: Sem r [(Expression, CFunType)] =
            mapM (bimapM asConstructor typeToFunType) (zip ctorArgs ctorInfo')
          patternCtorArgBindings :: Sem r [(Internal.Pattern, (Expression, CFunType))] = zip ps <$> ctorArgBindings
      patternCtorArgBindings >>= concatMapM go
      where
        ctorInfo :: Sem r [Internal.PolyType]
        ctorInfo = do
          p' :: HashMap Internal.Name Internal.ConstructorInfo <- asks (^. Internal.infoConstructors)
          let fInfo = HashMap.lookupDefault impossible constructorName p'
          return $ map mkPolyType' (fInfo ^. Internal.constructorInfoArgs)

        asConstructor :: Text -> Sem r Expression
        asConstructor ctorArg = do
          name <- getConstructorCName constructorName
          ty <- typeOfConstructor constructorName
          return (functionCall (ExpressionVar (asProjName ctorArg name)) [castToType ty exp])

getType ::
  Members '[Reader Internal.InfoTable, Reader Internal.TypesTable, Reader PatternInfoTable] r =>
  Internal.Iden ->
  Sem r (CFunType, CArity)
getType = \case
  Internal.IdenFunction n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoFunctions)
    funTyp <- typeToFunType (mkPolyType' (fInfo ^. (Internal.functionInfoDef . Internal.funDefType)))
    nPatterns <- functionInfoPatternsNum fInfo
    return (funTyp, nPatterns)
  Internal.IdenConstructor n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoConstructors)
    argTypes <- mapM (goType . mkPolyType') (fInfo ^. Internal.constructorInfoArgs)
    typ <- goType $ mkPolyType' (Internal.ExpressionIden (Internal.IdenInductive (fInfo ^. Internal.constructorInfoInductive)))
    return
      ( CFunType
          { _cFunArgTypes = argTypes,
            _cFunReturnType = typ
          },
        length argTypes
      )
  Internal.IdenAxiom n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Internal.infoAxioms)
    t <- typeToFunType (mkPolyType' (fInfo ^. Internal.axiomInfoType))
    return (t, length (t ^. cFunArgTypes))
  Internal.IdenVar n -> do
    t <- (^. bindingInfoType) . HashMap.lookupDefault impossible (n ^. Internal.nameText) <$> asks (^. patternBindings)
    return (t, length (t ^. cFunArgTypes))
  Internal.IdenInductive _ -> impossible
