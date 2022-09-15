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
import Juvix.Compiler.Internal.Extra qualified as Micro
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Micro
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

unsupported :: Text -> a
unsupported msg = error (msg <> " Micro to C: not yet supported")

-- The direct use of Micro.PolyType is safe here
unfoldFunType :: Micro.PolyType -> ([Micro.PolyType], Micro.PolyType)
unfoldFunType t = (map (Micro.PolyType . (^. Micro.paramType)) params, Micro.PolyType ret)
  where
    (params, ret) = Micro.unfoldFunType (t ^. Micro.unpolyType)

unfoldPolyApp :: Member (Reader Micro.TypesTable) r => Micro.Application -> Sem r (Micro.Expression, [Micro.Expression])
unfoldPolyApp a =
  let (f, args) = Micro.unfoldApplication a
   in case f of
        Micro.ExpressionLiteral {} -> return (f, toList args)
        Micro.ExpressionIden iden -> do
          args' <- filterCompileTimeArgsOrPatterns (Micro.getName iden) (toList args)
          return (f, args')
        _ -> impossible

filterCompileTimeArgsOrPatterns :: Member (Reader Micro.TypesTable) r => Micro.Name -> [a] -> Sem r [a]
filterCompileTimeArgsOrPatterns idenName lst = do
  tab <- ask
  return $
    map fst $
      filter (not . isUniverse . snd) $
        zip lst (map (^. Micro.paramType) (fst (Micro.unfoldFunType (ty tab))))
  where
    ty = HashMap.lookupDefault impossible idenName
    isUniverse :: Micro.Expression -> Bool
    isUniverse = \case
      (Micro.ExpressionUniverse {}) -> True
      _ -> False

mkName :: Micro.Name -> Text
mkName n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | T.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = T.filter isValidChar (n ^. Micro.nameText)
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
        capital = case n ^. Micro.nameKind of
          Micro.KNameConstructor -> True
          Micro.KNameInductive -> True
          Micro.KNameTopModule -> True
          Micro.KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Micro.nameKind of
      Micro.KNameTopModule -> mempty
      Micro.KNameFunction ->
        if n ^. Micro.nameText == Str.main then mempty else idSuffix
      _ -> idSuffix
    idSuffix :: Text
    idSuffix = "_" <> show (n ^. Micro.nameId . Micro.unNameId)

goType :: forall r. Member (Reader Micro.InfoTable) r => Micro.PolyType -> Sem r CDeclType
goType t = case t ^. Micro.unpolyType of
  Micro.ExpressionIden ti -> getMicroType ti
  Micro.ExpressionFunction {} -> return declFunctionPtrType
  Micro.ExpressionUniverse {} -> unsupported "TypeUniverse"
  Micro.ExpressionApplication a -> goType (mkPolyType' (fst (Micro.unfoldApplication a)))
  Micro.ExpressionLiteral {} -> impossible
  Micro.ExpressionHole {} -> impossible
  Micro.ExpressionSimpleLambda {} -> impossible
  Micro.ExpressionLambda {} -> impossible
  where
    getMicroType :: Micro.Iden -> Sem r CDeclType
    getMicroType = \case
      Micro.IdenInductive mn -> getInductiveCType mn
      Micro.IdenAxiom mn -> do
        axiomName <- getAxiomCName mn
        return
          CDeclType
            { _typeDeclType = DeclTypeDefType axiomName,
              _typeIsPtr = False
            }
      Micro.IdenVar {} ->
        return
          CDeclType
            { _typeDeclType = uIntPtrType,
              _typeIsPtr = False
            }
      _ -> impossible

typeToFunType :: Member (Reader Micro.InfoTable) r => Micro.PolyType -> Sem r CFunType
typeToFunType t = do
  let (args, ret) = unfoldFunType t
  _cFunArgTypes <- mapM goType args
  _cFunReturnType <- goType ret
  return CFunType {..}

applyOnFunStatement ::
  forall a. Monoid a => (Micro.FunctionDef -> a) -> Micro.Statement -> a
applyOnFunStatement f = \case
  Micro.StatementFunction x -> f x
  Micro.StatementForeign {} -> mempty
  Micro.StatementAxiom {} -> mempty
  Micro.StatementInductive {} -> mempty
  Micro.StatementInclude i -> mconcat $ map (applyOnFunStatement f) (i ^. Micro.includeModule . Micro.moduleBody . Micro.moduleStatements)

getConstructorCName :: Members '[Reader Micro.InfoTable] r => Micro.Name -> Sem r Text
getConstructorCName n = do
  ctorInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoConstructors)
  return
    ( case ctorInfo ^. Micro.constructorInfoBuiltin of
        Just builtin -> fromJust (builtinConstructorName builtin)
        Nothing -> mkName n
    )

getAxiomCName :: Members '[Reader Micro.InfoTable] r => Micro.Name -> Sem r Text
getAxiomCName n = do
  axiomInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoAxioms)
  return
    ( case axiomInfo ^. Micro.axiomInfoBuiltin of
        Just builtin -> fromJust (builtinAxiomName builtin)
        Nothing -> mkName n
    )

getInductiveCName :: Members '[Reader Micro.InfoTable] r => Micro.Name -> Sem r (Bool, Text)
getInductiveCName n = do
  inductiveInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoInductives)
  return
    ( case inductiveInfo ^. (Micro.inductiveInfoDef . Micro.inductiveBuiltin) of
        Just builtin -> (False, fromJust (builtinInductiveName builtin))
        Nothing -> (True, asTypeDef (mkName n))
    )

getInductiveCType :: Member (Reader Micro.InfoTable) r => Micro.Name -> Sem r CDeclType
getInductiveCType n = do
  (isPtr, name) <- getInductiveCName n
  return
    ( CDeclType
        { _typeDeclType = DeclTypeDefType name,
          _typeIsPtr = isPtr
        }
    )

typeOfConstructor :: Member (Reader Micro.InfoTable) r => Micro.Name -> Sem r CDeclType
typeOfConstructor name = do
  info <- Micro.lookupConstructor name
  getInductiveCType (info ^. Micro.constructorInfoInductive)

getClausePatterns :: Member (Reader Micro.TypesTable) r => Micro.FunctionClause -> Sem r [Micro.Pattern]
getClausePatterns c =
  filterCompileTimeArgsOrPatterns
    (c ^. Micro.clauseName)
    ( c
        ^.. Micro.clausePatterns
          . each
          . Micro.patternArgPattern
    )

functionInfoPatternsNum :: Member (Reader Micro.TypesTable) r => Micro.FunctionInfo -> Sem r Int
functionInfoPatternsNum fInfo = do
  let c = head (fInfo ^. (Micro.functionInfoDef . Micro.funDefClauses))
  pats <- getClausePatterns c
  return (length pats)

buildPatternInfoTable :: forall r. Members '[Reader Micro.InfoTable, Reader Micro.TypesTable] r => [Micro.PolyType] -> Micro.FunctionClause -> Sem r PatternInfoTable
buildPatternInfoTable argTyps c =
  PatternInfoTable . HashMap.fromList <$> patBindings
  where
    funArgBindings :: Sem r [(Expression, CFunType)]
    funArgBindings = mapM (bimapM (return . ExpressionVar) typeToFunType) (zip funArgs argTyps)

    patArgBindings :: Sem r [(Micro.Pattern, (Expression, CFunType))]
    patArgBindings = do
      pats <- getClausePatterns c
      zip pats <$> funArgBindings

    patBindings :: Sem r [(Text, BindingInfo)]
    patBindings = patArgBindings >>= concatMapM go

    go :: (Micro.Pattern, (Expression, CFunType)) -> Sem r [(Text, BindingInfo)]
    go (p, (exp, typ)) = case p of
      Micro.PatternVariable v ->
        return
          [(v ^. Micro.nameText, BindingInfo {_bindingInfoExpr = exp, _bindingInfoType = typ})]
      Micro.PatternConstructorApp Micro.ConstructorApp {..} ->
        goConstructorApp exp _constrAppConstructor (_constrAppParameters ^.. each . Micro.patternArgPattern)
      Micro.PatternWildcard {} -> return []

    goConstructorApp :: Expression -> Micro.Name -> [Micro.Pattern] -> Sem r [(Text, BindingInfo)]
    goConstructorApp exp constructorName ps = do
      ctorInfo' <- ctorInfo
      let ctorArgBindings :: Sem r [(Expression, CFunType)] =
            mapM (bimapM asConstructor typeToFunType) (zip ctorArgs ctorInfo')
          patternCtorArgBindings :: Sem r [(Micro.Pattern, (Expression, CFunType))] = zip ps <$> ctorArgBindings
      patternCtorArgBindings >>= concatMapM go
      where
        ctorInfo :: Sem r [Micro.PolyType]
        ctorInfo = do
          p' :: HashMap Micro.Name Micro.ConstructorInfo <- asks (^. Micro.infoConstructors)
          let fInfo = HashMap.lookupDefault impossible constructorName p'
          return $ map mkPolyType' (fInfo ^. Micro.constructorInfoArgs)

        asConstructor :: Text -> Sem r Expression
        asConstructor ctorArg = do
          name <- getConstructorCName constructorName
          ty <- typeOfConstructor constructorName
          return (functionCall (ExpressionVar (asProjName ctorArg name)) [castToType ty exp])

getType ::
  Members '[Reader Micro.InfoTable, Reader Micro.TypesTable, Reader PatternInfoTable] r =>
  Micro.Iden ->
  Sem r (CFunType, CArity)
getType = \case
  Micro.IdenFunction n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoFunctions)
    funTyp <- typeToFunType (mkPolyType' (fInfo ^. (Micro.functionInfoDef . Micro.funDefType)))
    nPatterns <- functionInfoPatternsNum fInfo
    return (funTyp, nPatterns)
  Micro.IdenConstructor n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoConstructors)
    argTypes <- mapM (goType . mkPolyType') (fInfo ^. Micro.constructorInfoArgs)
    typ <- goType $ mkPolyType' (Micro.ExpressionIden (Micro.IdenInductive (fInfo ^. Micro.constructorInfoInductive)))
    return
      ( CFunType
          { _cFunArgTypes = argTypes,
            _cFunReturnType = typ
          },
        length argTypes
      )
  Micro.IdenAxiom n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Micro.infoAxioms)
    t <- typeToFunType (mkPolyType' (fInfo ^. Micro.axiomInfoType))
    return (t, length (t ^. cFunArgTypes))
  Micro.IdenVar n -> do
    t <- (^. bindingInfoType) . HashMap.lookupDefault impossible (n ^. Micro.nameText) <$> asks (^. patternBindings)
    return (t, length (t ^. cFunArgTypes))
  Micro.IdenInductive _ -> impossible
