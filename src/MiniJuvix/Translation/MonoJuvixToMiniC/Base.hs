module MiniJuvix.Translation.MonoJuvixToMiniC.Base
  ( module MiniJuvix.Translation.MonoJuvixToMiniC.Base,
    module MiniJuvix.Translation.MonoJuvixToMiniC.Types,
    module MiniJuvix.Translation.MonoJuvixToMiniC.CNames,
    module MiniJuvix.Translation.MonoJuvixToMiniC.CBuilder,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language qualified as Micro
import MiniJuvix.Syntax.MiniC.Language
import MiniJuvix.Syntax.MonoJuvix.Language qualified as Mono
import MiniJuvix.Translation.MicroJuvixToMonoJuvix qualified as Mono
import MiniJuvix.Translation.MonoJuvixToMiniC.CBuilder
import MiniJuvix.Translation.MonoJuvixToMiniC.CNames
import MiniJuvix.Translation.MonoJuvixToMiniC.Types

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

goType :: Mono.Type -> CDeclType
goType t = case t of
  Mono.TypeIden ti -> getMonoType ti
  Mono.TypeFunction {} -> declFunctionPtrType
  Mono.TypeUniverse {} -> unsupported "TypeUniverse"
  where
    getMonoType :: Mono.TypeIden -> CDeclType
    getMonoType = \case
      Mono.TypeIdenInductive mn ->
        CDeclType
          { _typeDeclType = DeclTypeDefType (asTypeDef (mkName mn)),
            _typeIsPtr = True
          }
      Mono.TypeIdenAxiom mn ->
        CDeclType
          { _typeDeclType = DeclTypeDefType (mkName mn),
            _typeIsPtr = False
          }

typeToFunType :: Mono.Type -> CFunType
typeToFunType t =
  let (_cFunArgTypes, _cFunReturnType) =
        bimap (map goType) goType (unfoldFunType t)
   in CFunType {..}

applyOnFunStatement ::
  forall a. Monoid a => (Mono.FunctionDef -> a) -> Mono.Statement -> a
applyOnFunStatement f = \case
  Mono.StatementFunction x -> f x
  Mono.StatementForeign {} -> mempty
  Mono.StatementAxiom {} -> mempty
  Mono.StatementInductive {} -> mempty

buildPatternInfoTable :: forall r. Member (Reader Mono.InfoTable) r => [Mono.Type] -> Mono.FunctionClause -> Sem r PatternInfoTable
buildPatternInfoTable argTyps Mono.FunctionClause {..} =
  PatternInfoTable . HashMap.fromList <$> patBindings
  where
    funArgBindings :: [(Expression, CFunType)]
    funArgBindings = bimap ExpressionVar typeToFunType <$> zip funArgs argTyps

    patArgBindings :: [(Mono.Pattern, (Expression, CFunType))]
    patArgBindings = zip _clausePatterns funArgBindings

    patBindings :: Sem r [(Text, BindingInfo)]
    patBindings = concatMapM go patArgBindings

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
      let ctorArgBindings :: [(Expression, CFunType)] =
            bimap (memberAccess Object asConstructor) typeToFunType <$> zip ctorArgs ctorInfo'
          patternCtorArgBindings :: [(Mono.Pattern, (Expression, CFunType))] = zip ps ctorArgBindings
      concatMapM go patternCtorArgBindings
      where
        ctorInfo :: Sem r [Mono.Type]
        ctorInfo = do
          p' :: HashMap Mono.Name Mono.ConstructorInfo <- asks (^. Mono.infoConstructors)
          let fInfo = HashMap.lookupDefault impossible constructorName p'
          return $ fInfo ^. Mono.constructorInfoArgs

        asConstructor :: Expression
        asConstructor = functionCall (ExpressionVar (asCast (mkName constructorName))) [exp]

getType ::
  Members '[Reader Mono.InfoTable, Reader PatternInfoTable] r =>
  Mono.Iden ->
  Sem r (CFunType, CArity)
getType = \case
  Mono.IdenFunction n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoFunctions)
    return (typeToFunType (fInfo ^. Mono.functionInfoType), fInfo ^. Mono.functionInfoPatterns)
  Mono.IdenConstructor n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoConstructors)
    let argTypes = goType <$> (fInfo ^. Mono.constructorInfoArgs)
    return
      ( CFunType
          { _cFunArgTypes = argTypes,
            _cFunReturnType =
              goType
                (Mono.TypeIden (Mono.TypeIdenInductive (fInfo ^. Mono.constructorInfoInductive)))
          },
        length argTypes
      )
  Mono.IdenAxiom n -> do
    fInfo <- HashMap.lookupDefault impossible n <$> asks (^. Mono.infoAxioms)
    let t = typeToFunType (fInfo ^. Mono.axiomInfoType)
    return (t, length (t ^. cFunArgTypes))
  Mono.IdenVar n -> do
    t <- (^. bindingInfoType) . HashMap.lookupDefault impossible (n ^. Mono.nameText) <$> asks (^. patternBindings)
    return (t, length (t ^. cFunArgTypes))
