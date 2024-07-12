module Juvix.Compiler.Backend.Isabelle.Translation.FromTyped where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Data.Text qualified as Text
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Extra
import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Extra.Paths qualified as P

newtype NameSet = NameSet
  { _nameSet :: HashSet Text
  }

newtype NameMap = NameMap
  { _nameMap :: HashMap Name Name
  }

makeLenses ''NameSet
makeLenses ''NameMap

fromInternal ::
  forall r.
  (Members '[Error JuvixError, Reader EntryPoint, Reader ModuleTable, NameIdGen] r) =>
  Internal.InternalTypedResult ->
  Sem r Result
fromInternal Internal.InternalTypedResult {..} = do
  onlyTypes <- (^. entryPointIsabelleOnlyTypes) <$> ask
  itab <- getInternalModuleTable <$> ask
  let md :: Internal.InternalModule
      md = _resultInternalModule
      itab' :: Internal.InternalModuleTable
      itab' = Internal.insertInternalModule itab md
      table :: Internal.InfoTable
      table = Internal.computeCombinedInfoTable itab'
  go onlyTypes table _resultModule
  where
    go :: Bool -> Internal.InfoTable -> Internal.Module -> Sem r Result
    go onlyTypes tab md =
      return $
        Result
          { _resultTheory = goModule onlyTypes tab md,
            _resultModuleId = md ^. Internal.moduleId
          }

goModule :: Bool -> Internal.InfoTable -> Internal.Module -> Theory
goModule onlyTypes infoTable Internal.Module {..} =
  Theory
    { _theoryName = over nameText toIsabelleName $ over namePretty toIsabelleName _moduleName,
      _theoryImports = map (^. Internal.importModuleName) (_moduleBody ^. Internal.moduleImports),
      _theoryStatements = concatMap goMutualBlock (_moduleBody ^. Internal.moduleStatements)
    }
  where
    toIsabelleName :: Text -> Text
    toIsabelleName name = case reverse $ filter (/= "") $ T.splitOn "." name of
      h : _ -> h
      [] -> impossible

    isTypeDef :: Statement -> Bool
    isTypeDef = \case
      StmtDefinition {} -> False
      StmtFunction {} -> False
      StmtSynonym {} -> True
      StmtDatatype {} -> True
      StmtRecord {} -> True

    goMutualBlock :: Internal.MutualBlock -> [Statement]
    goMutualBlock Internal.MutualBlock {..} =
      filter (\stmt -> not onlyTypes || isTypeDef stmt) $
        map goMutualStatement (toList _mutualStatements)

    goMutualStatement :: Internal.MutualStatement -> Statement
    goMutualStatement = \case
      Internal.StatementInductive x -> goInductiveDef x
      Internal.StatementFunction x -> goFunctionDef x
      Internal.StatementAxiom x -> goAxiomDef x

    goInductiveDef :: Internal.InductiveDef -> Statement
    goInductiveDef Internal.InductiveDef {..}
      | length _inductiveConstructors == 1
          && head' _inductiveConstructors ^. Internal.inductiveConstructorIsRecord =
          let tyargs = fst $ Internal.unfoldFunType $ head' _inductiveConstructors ^. Internal.inductiveConstructorType
           in StmtRecord
                Record
                  { _recordName = _inductiveName,
                    _recordParams = params,
                    _recordFields = map goRecordField tyargs
                  }
      | otherwise =
          StmtDatatype
            Datatype
              { _datatypeName = _inductiveName,
                _datatypeParams = params,
                _datatypeConstructors = map goConstructorDef _inductiveConstructors
              }
      where
        params = map goInductiveParameter _inductiveParameters

    goInductiveParameter :: Internal.InductiveParameter -> TypeVar
    goInductiveParameter Internal.InductiveParameter {..} = TypeVar _inductiveParamName

    goRecordField :: Internal.FunctionParameter -> RecordField
    goRecordField Internal.FunctionParameter {..} =
      RecordField
        { _recordFieldName = fromMaybe (defaultName "_") _paramName,
          _recordFieldType = goType _paramType
        }

    goConstructorDef :: Internal.ConstructorDef -> Constructor
    goConstructorDef Internal.ConstructorDef {..} =
      Constructor
        { _constructorName = _inductiveConstructorName,
          _constructorArgTypes = tyargs
        }
      where
        tyargs = map (goType . (^. Internal.paramType)) (fst $ Internal.unfoldFunType _inductiveConstructorType)

    goDef :: Name -> Internal.Expression -> [Internal.ArgInfo] -> Maybe Internal.Expression -> Statement
    goDef name ty argsInfo body = case ty of
      Internal.ExpressionUniverse {} ->
        StmtSynonym
          Synonym
            { _synonymName = name',
              _synonymType = goType $ fromMaybe (error "unsupported axiomatic type") body
            }
      _
        | isFunction argnames ty body ->
            StmtFunction
              Function
                { _functionName = name',
                  _functionType = goType ty,
                  _functionClauses = goBody argnames ty body
                }
        | otherwise ->
            StmtDefinition
              Definition
                { _definitionName = name',
                  _definitionType = goType ty,
                  _definitionBody = maybe ExprUndefined goExpression' body
                }
      where
        argnames =
          map (overNameText quote) $ filterTypeArgs 0 ty $ map (fromMaybe (defaultName "_") . (^. Internal.argInfoName)) argsInfo
        name' = overNameText quote name

    isFunction :: [Name] -> Internal.Expression -> Maybe Internal.Expression -> Bool
    isFunction argnames ty = \case
      Just (Internal.ExpressionLambda Internal.Lambda {..})
        | not $ null $ filterTypeArgs 0 ty $ toList $ head _lambdaClauses ^. Internal.lambdaPatterns ->
            True
      _ -> not (null argnames)

    goBody :: [Name] -> Internal.Expression -> Maybe Internal.Expression -> NonEmpty Clause
    goBody argnames ty = \case
      Nothing -> oneClause ExprUndefined
      -- We assume here that all clauses have the same number of patterns
      Just (Internal.ExpressionLambda Internal.Lambda {..})
        | not $ null $ filterTypeArgs 0 ty $ toList $ head _lambdaClauses ^. Internal.lambdaPatterns ->
            fmap goClause _lambdaClauses
        | otherwise ->
            oneClause (goExpression'' nset (NameMap mempty) (head _lambdaClauses ^. Internal.lambdaBody))
      Just body -> oneClause (goExpression'' nset (NameMap mempty) body)
      where
        nset :: NameSet
        nset = NameSet $ HashSet.fromList $ map (^. namePretty) argnames

        oneClause :: Expression -> NonEmpty Clause
        oneClause expr =
          nonEmpty'
            [ Clause
                { _clausePatterns = nonEmpty' $ map PatVar argnames,
                  _clauseBody = expr
                }
            ]

        goClause :: Internal.LambdaClause -> Clause
        goClause Internal.LambdaClause {..} =
          Clause
            { _clausePatterns = nonEmpty' pats,
              _clauseBody = goExpression'' nset' nmap' _lambdaBody
            }
          where
            (pats, nset', nmap') = goPatternArgs'' (filterTypeArgs 0 ty (toList _lambdaPatterns))

    goFunctionDef :: Internal.FunctionDef -> Statement
    goFunctionDef Internal.FunctionDef {..} = goDef _funDefName _funDefType _funDefArgsInfo (Just _funDefBody)

    goAxiomDef :: Internal.AxiomDef -> Statement
    goAxiomDef Internal.AxiomDef {..} = goDef _axiomName _axiomType [] Nothing

    goType :: Internal.Expression -> Type
    goType ty = case ty of
      Internal.ExpressionIden x -> goTypeIden x
      Internal.ExpressionApplication x -> goTypeApp x
      Internal.ExpressionFunction x -> goTypeFun x
      Internal.ExpressionLiteral {} -> unsupportedType ty
      Internal.ExpressionHole {} -> unsupportedType ty
      Internal.ExpressionInstanceHole {} -> unsupportedType ty
      Internal.ExpressionLet {} -> unsupportedType ty
      Internal.ExpressionUniverse {} -> unsupportedType ty
      Internal.ExpressionSimpleLambda {} -> unsupportedType ty
      Internal.ExpressionLambda {} -> unsupportedType ty
      Internal.ExpressionCase {} -> unsupportedType ty
      where
        unsupportedType :: Internal.Expression -> a
        unsupportedType e = error ("unsupported type: " <> Internal.ppTrace e)

    mkIndType :: Name -> [Type] -> Type
    mkIndType name params = TyInd $ IndApp ind params
      where
        ind = case HashMap.lookup name (infoTable ^. Internal.infoInductives) of
          Just ii -> case ii ^. Internal.inductiveInfoBuiltin of
            Just Internal.BuiltinBool -> IndBool
            Just Internal.BuiltinNat -> IndNat
            Just Internal.BuiltinInt -> IndInt
            Just Internal.BuiltinList -> IndList
            Just Internal.BuiltinMaybe -> IndOption
            Just Internal.BuiltinPair -> IndTuple
            _ -> IndUser name
          Nothing -> case HashMap.lookup name (infoTable ^. Internal.infoAxioms) of
            Just ai -> case ai ^. Internal.axiomInfoDef . Internal.axiomBuiltin of
              Just Internal.BuiltinString -> IndString
              _ -> IndUser name
            Nothing -> IndUser name

    goTypeIden :: Internal.Iden -> Type
    goTypeIden = \case
      Internal.IdenFunction name -> mkIndType (overNameText quote name) []
      Internal.IdenConstructor name -> error ("unsupported type: constructor " <> Internal.ppTrace name)
      Internal.IdenVar name -> TyVar $ TypeVar (overNameText quote name)
      Internal.IdenAxiom name -> mkIndType (overNameText quote name) []
      Internal.IdenInductive name -> mkIndType (overNameText quote name) []

    goTypeApp :: Internal.Application -> Type
    goTypeApp app = mkIndType name params
      where
        (ind, args) = Internal.unfoldApplication app
        params = map goType (toList args)
        name = overNameText quote $ case ind of
          Internal.ExpressionIden (Internal.IdenFunction n) -> n
          Internal.ExpressionIden (Internal.IdenAxiom n) -> n
          Internal.ExpressionIden (Internal.IdenInductive n) -> n
          _ -> error ("unsupported type: " <> Internal.ppTrace app)

    goTypeFun :: Internal.Function -> Type
    goTypeFun Internal.Function {..}
      | Internal.isTypeConstructor lty = goType _functionRight
      | otherwise =
          TyFun $ FunType (goType lty) (goType _functionRight)
      where
        lty = _functionLeft ^. Internal.paramType

    goConstrName :: Name -> Name
    goConstrName name =
      case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
        Just ctrInfo ->
          case ctrInfo ^. Internal.constructorInfoBuiltin of
            Just Internal.BuiltinNatSuc ->
              setNameText "Suc" name
            Just Internal.BuiltinBoolTrue ->
              setNameText "True" name
            Just Internal.BuiltinBoolFalse ->
              setNameText "False" name
            Just Internal.BuiltinMaybeNothing ->
              setNameText "None" name
            Just Internal.BuiltinMaybeJust ->
              setNameText "Some" name
            _ -> overNameText quote name
        Nothing -> overNameText quote name

    goFunName :: Name -> Name
    goFunName name =
      case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
        Just funInfo ->
          case funInfo ^. Internal.functionInfoPragmas . pragmasIsabelleFunction of
            Just PragmaIsabelleFunction {..} -> setNameText _pragmaIsabelleFunctionName name
            Nothing -> overNameText quote name
        Nothing -> overNameText quote name

    lookupName :: forall r. (Member (Reader NameMap) r) => Name -> Sem r Name
    lookupName name = do
      nmap <- asks (^. nameMap)
      return $ fromMaybe name $ HashMap.lookup name nmap

    localName :: forall a r. (Members '[Reader NameSet, Reader NameMap] r) => Name -> Name -> Sem r a -> Sem r a
    localName v v' =
      local (over nameSet (HashSet.insert (v' ^. namePretty)))
        . local (over nameMap (HashMap.insert v v'))

    localNames :: forall a r. (Members '[Reader NameSet, Reader NameMap] r) => [(Name, Name)] -> Sem r a -> Sem r a
    localNames vs e = foldl' (flip (uncurry localName)) e vs

    withLocalNames :: forall a r. (Members '[Reader NameSet, Reader NameMap] r) => NameSet -> NameMap -> Sem r a -> Sem r a
    withLocalNames nset nmap =
      local (const nset) . local (const nmap)

    goExpression' :: Internal.Expression -> Expression
    goExpression' = goExpression'' (NameSet mempty) (NameMap mempty)

    goExpression'' :: NameSet -> NameMap -> Internal.Expression -> Expression
    goExpression'' nset nmap e =
      run $ runReader nset $ runReader nmap $ goExpression e

    goExpression :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => Internal.Expression -> Sem r Expression
    goExpression = \case
      Internal.ExpressionIden x -> goIden x
      Internal.ExpressionApplication x -> goApplication x
      Internal.ExpressionFunction x -> goFunType x
      Internal.ExpressionLiteral x -> goLiteral x
      Internal.ExpressionHole x -> goHole x
      Internal.ExpressionInstanceHole x -> goInstanceHole x
      Internal.ExpressionLet x -> goLet x
      Internal.ExpressionUniverse x -> goUniverse x
      Internal.ExpressionSimpleLambda x -> goSimpleLambda x
      Internal.ExpressionLambda x -> goLambda x
      Internal.ExpressionCase x -> goCase x
      where
        goIden :: Internal.Iden -> Sem r Expression
        goIden iden = case iden of
          Internal.IdenFunction name -> do
            name' <- lookupName name
            return $ ExprIden (goFunName name')
          Internal.IdenConstructor name ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinNatZero -> return $ ExprLiteral (LitNumeric 0)
                  _ -> return $ ExprIden (goConstrName name)
              Nothing -> return $ ExprIden (goConstrName name)
          Internal.IdenVar name -> do
            name' <- lookupName name
            return $ ExprIden name'
          Internal.IdenAxiom name -> return $ ExprIden (overNameText quote name)
          Internal.IdenInductive name -> return $ ExprIden (overNameText quote name)

        goApplication :: Internal.Application -> Sem r Expression
        goApplication app@Internal.Application {..}
          | Just (pragmas, arg1, arg2) <- getIsabelleOperator app =
              mkIsabelleOperator pragmas arg1 arg2
          | Just x <- getLiteral app =
              return $ ExprLiteral $ LitNumeric x
          | Just xs <- getList app = do
              xs' <- mapM goExpression xs
              return $ ExprList (List xs')
          | Just (arg1, arg2) <- getCons app = do
              arg1' <- goExpression arg1
              arg2' <- goExpression arg2
              return $ ExprCons $ Cons arg1' arg2'
          | Just (val, br1, br2) <- getIf app = do
              val' <- goExpression val
              br1' <- goExpression br1
              br2' <- goExpression br2
              return $ ExprIf $ If val' br1' br2'
          | Just (op, fixity, arg1, arg2) <- getBoolOperator app = do
              arg1' <- goExpression arg1
              arg2' <- goExpression arg2
              return $
                ExprBinop
                  Binop
                    { _binopOperator = op,
                      _binopLeft = arg1',
                      _binopRight = arg2',
                      _binopFixity = fixity
                    }
          | Just (x, y) <- getPair app = do
              x' <- goExpression x
              y' <- goExpression y
              return $ ExprTuple (Tuple (x' :| [y']))
          | Just (fn, args) <- getIdentApp app = do
              fn' <- goExpression fn
              args' <- mapM goExpression args
              return $ mkApp fn' args'
          | otherwise = do
              l <- goExpression _appLeft
              r <- goExpression _appRight
              return $ ExprApp (Application l r)

        mkIsabelleOperator :: PragmaIsabelleOperator -> Internal.Expression -> Internal.Expression -> Sem r Expression
        mkIsabelleOperator PragmaIsabelleOperator {..} arg1 arg2 = do
          arg1' <- goExpression arg1
          arg2' <- goExpression arg2
          return $
            ExprBinop
              Binop
                { _binopOperator = defaultName _pragmaIsabelleOperatorName,
                  _binopLeft = arg1',
                  _binopRight = arg2',
                  _binopFixity =
                    Fixity
                      { _fixityPrecedence = PrecNat (fromMaybe 0 _pragmaIsabelleOperatorPrec),
                        _fixityArity = OpBinary (fromMaybe AssocNone _pragmaIsabelleOperatorAssoc),
                        _fixityId = Nothing
                      }
                }

        getIsabelleOperator :: Internal.Application -> Maybe (PragmaIsabelleOperator, Internal.Expression, Internal.Expression)
        getIsabelleOperator app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoPragmas . pragmasIsabelleOperator of
                  Just pragma ->
                    case args of
                      Internal.ExpressionIden (Internal.IdenInductive tyname) :| [_, arg1, arg2] ->
                        case HashMap.lookup tyname (infoTable ^. Internal.infoInductives) of
                          Just Internal.InductiveInfo {..} ->
                            case _inductiveInfoBuiltin of
                              Just Internal.BuiltinNat -> Just (pragma, arg1, arg2)
                              Just Internal.BuiltinInt -> Just (pragma, arg1, arg2)
                              _ -> Nothing
                          Nothing -> Nothing
                      _ -> Nothing
                  Nothing -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getLiteral :: Internal.Application -> Maybe Integer
        getLiteral app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoBuiltin of
                  Just Internal.BuiltinFromNat -> lit
                  Just Internal.BuiltinFromInt -> lit
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

            lit :: Maybe Integer
            lit = case args of
              _ :| [_, Internal.ExpressionLiteral l] ->
                case l ^. Internal.withLocParam of
                  Internal.LitString {} -> Nothing
                  Internal.LitNumeric x -> Just x
                  Internal.LitInteger x -> Just x
                  Internal.LitNatural x -> Just x
              _ -> Nothing

        getList :: Internal.Application -> Maybe [Internal.Expression]
        getList app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinListNil -> Just []
                  Just Internal.BuiltinListCons
                    | (_ :| [arg1, Internal.ExpressionApplication app2]) <- args,
                      Just lst <- getList app2 ->
                        Just (arg1 : lst)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getCons :: Internal.Application -> Maybe (Internal.Expression, Internal.Expression)
        getCons app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinListCons
                    | (_ :| [arg1, arg2]) <- args ->
                        Just (arg1, arg2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getIf :: Internal.Application -> Maybe (Internal.Expression, Internal.Expression, Internal.Expression)
        getIf app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoBuiltin of
                  Just Internal.BuiltinBoolIf
                    | (_ :| [val, br1, br2]) <- args ->
                        Just (val, br1, br2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getBoolOperator :: Internal.Application -> Maybe (Name, Fixity, Internal.Expression, Internal.Expression)
        getBoolOperator app = case fn of
          Internal.ExpressionIden (Internal.IdenFunction name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
              Just funInfo ->
                case funInfo ^. Internal.functionInfoBuiltin of
                  Just Internal.BuiltinBoolAnd
                    | (arg1 :| [arg2]) <- args ->
                        Just (defaultName "∧", andFixity, arg1, arg2)
                  Just Internal.BuiltinBoolOr
                    | (arg1 :| [arg2]) <- args ->
                        Just (defaultName "∨", orFixity, arg1, arg2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getPair :: Internal.Application -> Maybe (Internal.Expression, Internal.Expression)
        getPair app = case fn of
          Internal.ExpressionIden (Internal.IdenConstructor name) ->
            case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
              Just ctrInfo ->
                case ctrInfo ^. Internal.constructorInfoBuiltin of
                  Just Internal.BuiltinPairConstr
                    | (_ :| [_, arg1, arg2]) <- args ->
                        Just (arg1, arg2)
                  _ -> Nothing
              Nothing -> Nothing
          _ -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app

        getIdentApp :: Internal.Application -> Maybe (Internal.Expression, [Internal.Expression])
        getIdentApp app = case mty of
          Just (ty, paramsNum) -> Just (fn, args')
            where
              args' = filterTypeArgs paramsNum ty (toList args)
          Nothing -> Nothing
          where
            (fn, args) = Internal.unfoldApplication app
            mty = case fn of
              Internal.ExpressionIden (Internal.IdenFunction name) ->
                case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
                  Just funInfo -> Just (funInfo ^. Internal.functionInfoType, 0)
                  Nothing -> Nothing
              Internal.ExpressionIden (Internal.IdenConstructor name) ->
                case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
                  Just ctrInfo ->
                    Just
                      ( ctrInfo ^. Internal.constructorInfoType,
                        length (ctrInfo ^. Internal.constructorInfoInductiveParameters)
                      )
                  Nothing -> Nothing
              _ -> Nothing

        goFunType :: Internal.Function -> Sem r Expression
        goFunType _ = return ExprUndefined

        goLiteral :: Internal.LiteralLoc -> Sem r Expression
        goLiteral lit = return $ ExprLiteral $ case lit ^. withLocParam of
          Internal.LitString s -> LitString s
          Internal.LitNumeric n -> LitNumeric n
          Internal.LitInteger n -> LitNumeric n
          Internal.LitNatural n -> LitNumeric n

        goHole :: Internal.Hole -> Sem r Expression
        goHole _ = return ExprUndefined

        goInstanceHole :: Internal.InstanceHole -> Sem r Expression
        goInstanceHole _ = return ExprUndefined

        -- TODO: binders
        goLet :: Internal.Let -> Sem r Expression
        goLet Internal.Let {..} = do
          let fdefs = concatMap toFunDefs (toList _letClauses)
          cls <- mapM goFunDef fdefs
          let ns = zipExact (map (^. Internal.funDefName) fdefs) (map (^. letClauseName) cls)
          expr <- localNames ns $ goExpression _letExpression
          return $
            ExprLet
              Let
                { _letClauses = nonEmpty' cls,
                  _letBody = expr
                }
          where
            toFunDefs :: Internal.LetClause -> [Internal.FunctionDef]
            toFunDefs = \case
              Internal.LetFunDef d -> [d]
              Internal.LetMutualBlock Internal.MutualBlockLet {..} -> toList _mutualLet

            goFunDef :: Internal.FunctionDef -> Sem r LetClause
            goFunDef Internal.FunctionDef {..} = do
              nset <- asks (^. nameSet)
              let name' = overNameText (disambiguate nset) _funDefName
              val <- localName _funDefName name' $ goExpression _funDefBody
              return $
                LetClause
                  { _letClauseName = name',
                    _letClauseValue = val
                  }

        goUniverse :: Internal.SmallUniverse -> Sem r Expression
        goUniverse _ = return ExprUndefined

        goSimpleLambda :: Internal.SimpleLambda -> Sem r Expression
        goSimpleLambda Internal.SimpleLambda {..} = do
          nset <- asks (^. nameSet)
          let v = _slambdaBinder ^. Internal.sbinderVar
              v' = overNameText (disambiguate nset) v
          body <-
            localName v v' $
              goExpression _slambdaBody
          return $
            ExprLambda
              Lambda
                { _lambdaVar = v',
                  _lambdaType = Just $ goType $ _slambdaBinder ^. Internal.sbinderType,
                  _lambdaBody = body
                }

        goLambda :: Internal.Lambda -> Sem r Expression
        goLambda Internal.Lambda {..}
          | npats == 0 = goExpression (head _lambdaClauses ^. Internal.lambdaBody)
          | otherwise = goLams vars
          where
            npats =
              case _lambdaType of
                Just ty ->
                  length
                    . filterTypeArgs 0 ty
                    . toList
                    $ head _lambdaClauses ^. Internal.lambdaPatterns
                Nothing ->
                  length
                    . filter ((/= Internal.Implicit) . (^. Internal.patternArgIsImplicit))
                    . toList
                    $ head _lambdaClauses ^. Internal.lambdaPatterns
            vars = map (\i -> defaultName ("x" <> show i)) [0 .. npats - 1]

            goLams :: [Name] -> Sem r Expression
            goLams = \case
              v : vs -> do
                nset <- asks (^. nameSet)
                let v' = overNameText (disambiguate nset) v
                body <-
                  localName v v' $
                    goLams vs
                return $
                  ExprLambda
                    Lambda
                      { _lambdaType = Nothing,
                        _lambdaVar = v',
                        _lambdaBody = body
                      }
              [] -> do
                val <-
                  case vars of
                    [v] -> do
                      v' <- lookupName v
                      return $ ExprIden v'
                    _ -> do
                      vars' <- mapM lookupName vars
                      return $
                        ExprTuple
                          Tuple
                            { _tupleComponents = nonEmpty' $ map ExprIden vars'
                            }
                brs <- mapM goClause _lambdaClauses
                return $
                  ExprCase
                    Case
                      { _caseValue = val,
                        _caseBranches = brs
                      }

            goClause :: Internal.LambdaClause -> Sem r CaseBranch
            goClause Internal.LambdaClause {..} = do
              (pat, nset, nmap) <- case _lambdaPatterns of
                p :| [] -> goPatternArg' p
                _ -> do
                  (pats, nset, nmap) <- goPatternArgs' (toList _lambdaPatterns)
                  let pat =
                        PatTuple
                          Tuple
                            { _tupleComponents = nonEmpty' pats
                            }
                  return (pat, nset, nmap)
              body <- withLocalNames nset nmap $ goExpression _lambdaBody
              return $
                CaseBranch
                  { _caseBranchPattern = pat,
                    _caseBranchBody = body
                  }

        goCase :: Internal.Case -> Sem r Expression
        goCase Internal.Case {..} = do
          val <- goExpression _caseExpression
          brs <- mapM goCaseBranch _caseBranches
          return $
            ExprCase
              Case
                { _caseValue = val,
                  _caseBranches = brs
                }

        goCaseBranch :: Internal.CaseBranch -> Sem r CaseBranch
        goCaseBranch Internal.CaseBranch {..} = do
          (pat, nset, nmap) <- goPatternArg' _caseBranchPattern
          rhs <- withLocalNames nset nmap $ goCaseBranchRhs _caseBranchRhs
          return $
            CaseBranch
              { _caseBranchPattern = pat,
                _caseBranchBody = rhs
              }

        goCaseBranchRhs :: Internal.CaseBranchRhs -> Sem r Expression
        goCaseBranchRhs = \case
          Internal.CaseBranchRhsExpression e -> goExpression e
          Internal.CaseBranchRhsIf {} -> error "unsupported: side conditions"

    goPatternArgs'' :: [Internal.PatternArg] -> ([Pattern], NameSet, NameMap)
    goPatternArgs'' pats =
      (pats', nset, nmap)
      where
        (nset, (nmap, pats')) = run $ runState (NameSet mempty) $ runState (NameMap mempty) $ goPatternArgs pats

    goPatternArg' :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => Internal.PatternArg -> Sem r (Pattern, NameSet, NameMap)
    goPatternArg' pat = do
      nset <- ask @NameSet
      nmap <- ask @NameMap
      let (nmap', (nset', pat')) = run $ runState nmap $ runState nset $ goPatternArg pat
      return (pat', nset', nmap')

    goPatternArgs' :: forall r. (Members '[Reader NameSet, Reader NameMap] r) => [Internal.PatternArg] -> Sem r ([Pattern], NameSet, NameMap)
    goPatternArgs' pats = do
      nset <- ask @NameSet
      nmap <- ask @NameMap
      let (nmap', (nset', pats')) = run $ runState nmap $ runState nset $ goPatternArgs pats
      return (pats', nset', nmap')

    goPatternArgs :: forall r. (Members '[State NameSet, State NameMap] r) => [Internal.PatternArg] -> Sem r [Pattern]
    goPatternArgs = mapM goPatternArg

    -- TODO: named patterns (`_patternArgName`) are not handled properly
    goPatternArg :: forall r. (Members '[State NameSet, State NameMap] r) => Internal.PatternArg -> Sem r Pattern
    goPatternArg Internal.PatternArg {..} =
      goPattern _patternArgPattern
      where
        goPattern :: Internal.Pattern -> Sem r Pattern
        goPattern = \case
          Internal.PatternVariable name -> do
            binders <- gets (^. nameSet)
            let name' = overNameText (disambiguate binders) name
            modify' (over nameSet (HashSet.insert (name' ^. namePretty)))
            modify' (over nameMap (HashMap.insert name name'))
            return $ PatVar name'
          Internal.PatternConstructorApp x -> goPatternConstructorApp x
          Internal.PatternWildcardConstructor {} -> impossible

        goPatternConstructorApp :: Internal.ConstructorApp -> Sem r Pattern
        goPatternConstructorApp Internal.ConstructorApp {..}
          | Just lst <- getListPat _constrAppConstructor _constrAppParameters = do
              pats <- goPatternArgs lst
              return $ PatList (List pats)
          | Just (x, y) <- getConsPat _constrAppConstructor _constrAppParameters = do
              x' <- goPatternArg x
              y' <- goPatternArg y
              return $ PatCons (Cons x' y')
          | Just (x, y) <- getPairPat _constrAppConstructor _constrAppParameters = do
              x' <- goPatternArg x
              y' <- goPatternArg y
              return $ PatTuple (Tuple (x' :| [y']))
          | Just p <- getNatPat _constrAppConstructor _constrAppParameters =
              case p of
                Left zero -> return zero
                Right arg -> do
                  arg' <- goPatternArg arg
                  return (PatConstrApp (ConstrApp (goConstrName _constrAppConstructor) [arg']))
          | otherwise = do
              args <- mapM goPatternArg _constrAppParameters
              return $
                PatConstrApp
                  ConstrApp
                    { _constrAppConstructor = goConstrName _constrAppConstructor,
                      _constrAppArgs = args
                    }

        -- This function cannot be simply merged with `getList` because in patterns
        -- the constructors don't get the type arguments.
        getListPat :: Name -> [Internal.PatternArg] -> Maybe [Internal.PatternArg]
        getListPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinListNil -> Just []
                Just Internal.BuiltinListCons
                  | [arg1, Internal.PatternArg {..}] <- args,
                    Internal.PatternConstructorApp Internal.ConstructorApp {..} <- _patternArgPattern,
                    Just lst <- getListPat _constrAppConstructor _constrAppParameters ->
                      Just (arg1 : lst)
                _ -> Nothing
            Nothing -> Nothing

        getConsPat :: Name -> [Internal.PatternArg] -> Maybe (Internal.PatternArg, Internal.PatternArg)
        getConsPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinListCons
                  | [arg1, arg2] <- args ->
                      Just (arg1, arg2)
                _ -> Nothing
            Nothing -> Nothing

        getNatPat :: Name -> [Internal.PatternArg] -> Maybe (Either Pattern Internal.PatternArg)
        getNatPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinNatZero
                  | null args ->
                      Just $ Left PatZero
                Just Internal.BuiltinNatSuc
                  | [arg] <- args ->
                      Just $ Right arg
                _ -> Nothing
            Nothing -> Nothing

        getPairPat :: Name -> [Internal.PatternArg] -> Maybe (Internal.PatternArg, Internal.PatternArg)
        getPairPat name args =
          case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
            Just funInfo ->
              case funInfo ^. Internal.constructorInfoBuiltin of
                Just Internal.BuiltinPairConstr
                  | [arg1, arg2] <- args ->
                      Just (arg1, arg2)
                _ -> Nothing
            Nothing -> Nothing

    defaultName :: Text -> Name
    defaultName n =
      Name
        { _nameText = n,
          _nameId = defaultId,
          _nameKind = KNameLocal,
          _nameKindPretty = KNameLocal,
          _namePretty = n,
          _nameLoc = defaultLoc,
          _nameFixity = Nothing
        }
      where
        defaultLoc = singletonInterval $ mkInitialLoc P.noFile
        defaultId =
          NameId
            { _nameIdUid = 0,
              _nameIdModuleId = defaultModuleId
            }

    setNameText :: Text -> Name -> Name
    setNameText txt name =
      set namePretty txt
        . set nameText txt
        $ name

    overNameText :: (Text -> Text) -> Name -> Name
    overNameText f name =
      over namePretty f
        . over nameText f
        $ name

    disambiguate :: HashSet Text -> Text -> Text
    disambiguate binders = disambiguate' binders . quote

    disambiguate' :: HashSet Text -> Text -> Text
    disambiguate' binders name
      | name == "?" || name == "" || name == "_" =
          disambiguate' binders "X"
      | HashSet.member name binders
          || HashSet.member name names =
          disambiguate' binders (prime name)
      | otherwise =
          name

    names :: HashSet Text
    names =
      HashSet.fromList $
        map (^. Internal.functionInfoName . namePretty) (filter (not . (^. Internal.functionInfoIsLocal)) (HashMap.elems (infoTable ^. Internal.infoFunctions)))
          ++ map (^. Internal.constructorInfoName . namePretty) (HashMap.elems (infoTable ^. Internal.infoConstructors))
          ++ map (^. Internal.inductiveInfoName . namePretty) (HashMap.elems (infoTable ^. Internal.infoInductives))
          ++ map (^. Internal.axiomInfoDef . Internal.axiomName . namePretty) (HashMap.elems (infoTable ^. Internal.infoAxioms))

    quote :: Text -> Text
    quote txt = case Text.uncons txt of
      Just ('_', txt') -> txt'
      _ -> txt

    filterTypeArgs :: Int -> Internal.Expression -> [a] -> [a]
    filterTypeArgs paramsNum ty args =
      map fst $
        filter (not . snd) $
          zip (drop paramsNum args) (argtys ++ repeat False)
      where
        argtys =
          map Internal.isTypeConstructor
            . map (^. Internal.paramType)
            . fst
            . Internal.unfoldFunType
            $ ty
