module Juvix.Translation.MonoJuvixToMiniC.Base
  ( module Juvix.Translation.MonoJuvixToMiniC.Base,
    module Juvix.Translation.MonoJuvixToMiniC.Types,
    module Juvix.Translation.MonoJuvixToMiniC.CNames,
    module Juvix.Translation.MonoJuvixToMiniC.CBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Juvix.Internal.Strings qualified as Str
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language qualified as Micro
import Juvix.Syntax.MiniC.Language
import Juvix.Syntax.MonoJuvix.Language qualified as Mono
import Juvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import Juvix.Translation.MonoJuvixToMiniC.BuiltinTable
import Juvix.Translation.MonoJuvixToMiniC.CBuilder
import Juvix.Translation.MonoJuvixToMiniC.CNames
import Juvix.Translation.MonoJuvixToMiniC.Types

unsupported :: Text -> a
unsupported msg = error (msg <> " Mono to C: not yet supported")

unfoldFunType :: Mono.Type -> ([Mono.Type], Mono.Type)
unfoldFunType t = case t of
  Mono.TypeFunction (Mono.Function l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

mkName :: Mono.Name -> Text
mkName n =
  adaptFirstLetter lexeme <> nameTextSuffix
  where
    lexeme
      | T.null lexeme' = "v"
      | otherwise = lexeme'
      where
        lexeme' = T.filter isValidChar (n ^. Mono.nameText)
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
        capital = case n ^. Mono.nameKind of
          Mono.KNameConstructor -> True
          Mono.KNameInductive -> True
          Mono.KNameTopModule -> True
          Mono.KNameLocalModule -> True
          _ -> False
    nameTextSuffix :: Text
    nameTextSuffix = case n ^. Mono.nameKind of
      Mono.KNameTopModule -> mempty
      Mono.KNameFunction ->
        if n ^. Mono.nameText == Str.main then mempty else idSuffix
      _ -> idSuffix
    idSuffix :: Text
    idSuffix = "_" <> show (n ^. Mono.nameId . Micro.unNameId)

goType :: forall r. Member (Reader Mono.InfoTable) r => Mono.Type -> Sem r CDeclType
goType t = case t of
  Mono.TypeIden ti -> getMonoType ti
  Mono.TypeFunction {} -> return declFunctionPtrType
  Mono.TypeUniverse {} -> unsupported "TypeUniverse"
  where
    getMonoType :: Mono.TypeIden -> Sem r CDeclType
    getMonoType = \case
      Mono.TypeIdenInductive mn -> do
        (isPtr, name) <- getInductiveCName mn
        return
          ( CDeclType
              { _typeDeclType = DeclTypeDefType name,
                _typeIsPtr = isPtr
              }
          )
      Mono.TypeIdenAxiom mn -> do
        axiomName <- getAxiomCName mn
        return
          CDeclType
            { _typeDeclType = DeclTypeDefType axiomName,
              _typeIsPtr = False
            }

typeToFunType :: Member (Reader Mono.InfoTable) r => Mono.Type -> Sem r CFunType
typeToFunType t = do
  let (args, ret) = unfoldFunType t
  _cFunArgTypes <- mapM goType args
  _cFunReturnType <- goType ret
  return CFunType {..}

applyOnFunStatement ::
  forall a. Monoid a => (Mono.FunctionDef -> a) -> Mono.Statement -> a
applyOnFunStatement f = \case
  Mono.StatementFunction x -> f x
  Mono.StatementForeign {} -> mempty
  Mono.StatementAxiom {} -> mempty
  Mono.StatementInductive {} -> mempty

getConstructorCName :: Members '[Reader Mono.InfoTable] r => Mono.Name -> Sem r Text
getConstructorCName n = do
  ctorInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoConstructors)
  return
    ( case ctorInfo ^. Mono.constructorInfoBuiltin of
        Just builtin -> fromJust (builtinConstructorName builtin)
        Nothing -> mkName n
    )

getAxiomCName :: Members '[Reader Mono.InfoTable] r => Mono.Name -> Sem r Text
getAxiomCName n = do
  axiomInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoAxioms)
  return
    ( case axiomInfo ^. Mono.axiomInfoBuiltin of
        Just builtin -> fromJust (builtinAxiomName builtin)
        Nothing -> mkName n
    )

getInductiveCName :: Members '[Reader Mono.InfoTable] r => Mono.Name -> Sem r (Bool, Text)
getInductiveCName n = do
  inductiveInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoInductives)
  return
    ( case inductiveInfo ^. Mono.inductiveInfoBuiltin of
        Just builtin -> (False, fromJust (builtinInductiveName builtin))
        Nothing -> (True, asTypeDef (mkName n))
    )

buildPatternInfoTable :: forall r. Member (Reader Mono.InfoTable) r => [Mono.Type] -> Mono.FunctionClause -> Sem r PatternInfoTable
buildPatternInfoTable argTyps Mono.FunctionClause {..} =
  PatternInfoTable . HashMap.fromList <$> patBindings
  where
    funArgBindings :: Sem r [(Expression, CFunType)]
    funArgBindings = mapM (bimapM (return . ExpressionVar) typeToFunType) (zip funArgs argTyps)

    patArgBindings :: Sem r [(Mono.Pattern, (Expression, CFunType))]
    patArgBindings = zip _clausePatterns <$> funArgBindings

    patBindings :: Sem r [(Text, BindingInfo)]
    patBindings = patArgBindings >>= concatMapM go

    go :: (Mono.Pattern, (Expression, CFunType)) -> Sem r [(Text, BindingInfo)]
    go (p, (exp, typ)) = case p of
      Mono.PatternVariable v ->
        return
          [(v ^. Mono.nameText, BindingInfo {_bindingInfoExpr = exp, _bindingInfoType = typ})]
      Mono.PatternConstructorApp Mono.ConstructorApp {..} ->
        goConstructorApp exp _constrAppConstructor _constrAppParameters
      Mono.PatternWildcard {} -> return []

    goConstructorApp :: Expression -> Mono.Name -> [Mono.Pattern] -> Sem r [(Text, BindingInfo)]
    goConstructorApp exp constructorName ps = do
      ctorInfo' <- ctorInfo
      let ctorArgBindings :: Sem r [(Expression, CFunType)] =
            mapM (bimapM asConstructor typeToFunType) (zip ctorArgs ctorInfo')
          patternCtorArgBindings :: Sem r [(Mono.Pattern, (Expression, CFunType))] = zip ps <$> ctorArgBindings
      patternCtorArgBindings >>= concatMapM go
      where
        ctorInfo :: Sem r [Mono.Type]
        ctorInfo = do
          p' :: HashMap Mono.Name Mono.ConstructorInfo <- asks (^. Mono.infoConstructors)
          let fInfo = HashMap.lookupDefault impossible constructorName p'
          return $ fInfo ^. Mono.constructorInfoArgs

        asConstructor :: Text -> Sem r Expression
        asConstructor ctorArg = do
          name <- getConstructorCName constructorName
          return (functionCall (ExpressionVar (asProjName ctorArg name)) [exp])

getType ::
  Members '[Reader Mono.InfoTable, Reader PatternInfoTable] r =>
  Mono.Iden ->
  Sem r (CFunType, CArity)
getType = \case
  Mono.IdenFunction n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoFunctions)
    funTyp <- typeToFunType (fInfo ^. Mono.functionInfoType)
    return (funTyp, fInfo ^. Mono.functionInfoPatterns)
  Mono.IdenConstructor n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoConstructors)
    argTypes <- mapM goType (fInfo ^. Mono.constructorInfoArgs)
    typ <- goType (Mono.TypeIden (Mono.TypeIdenInductive (fInfo ^. Mono.constructorInfoInductive)))
    return
      ( CFunType
          { _cFunArgTypes = argTypes,
            _cFunReturnType = typ
          },
        length argTypes
      )
  Mono.IdenAxiom n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoAxioms)
    t <- typeToFunType (fInfo ^. Mono.axiomInfoType)
    return (t, length (t ^. cFunArgTypes))
  Mono.IdenVar n -> do
    t <- (^. bindingInfoType) . HashMap.lookupDefault impossible (n ^. Mono.nameText) <$> asks (^. patternBindings)
    return (t, length (t ^. cFunArgTypes))
