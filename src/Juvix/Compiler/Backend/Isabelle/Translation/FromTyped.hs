module Juvix.Compiler.Backend.Isabelle.Translation.FromTyped where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Pretty qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Extra.Paths qualified as P

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
            { _synonymName = name,
              _synonymType = goType $ fromMaybe (error "unsupported axiomatic type") body
            }
      _
        | isFunction argnames body ->
            StmtFunction
              Function
                { _functionName = name,
                  _functionType = goType ty,
                  _functionClauses = goBody argnames body
                }
        | otherwise ->
            StmtDefinition
              Definition
                { _definitionName = name,
                  _definitionType = goType ty,
                  _definitionBody = maybe ExprUndefined goExpression body
                }
        where
          argnames = map (fromMaybe (defaultName "_") . (^. Internal.argInfoName)) argsInfo

    isFunction :: [Name] -> Maybe Internal.Expression -> Bool
    isFunction argnames = \case
      Just (Internal.ExpressionLambda {}) -> True
      _ -> not (null argnames)

    goBody :: [Name] -> Maybe Internal.Expression -> NonEmpty Clause
    goBody argnames = \case
      Nothing -> oneClause ExprUndefined
      Just (Internal.ExpressionLambda Internal.Lambda {..}) ->
        fmap goClause _lambdaClauses
      Just body -> oneClause (goExpression body)
      where
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
            { _clausePatterns = nonEmpty' $ map goPatternArg (toList _lambdaPatterns),
              _clauseBody = goExpression _lambdaBody
            }

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
            _ -> IndUser name
          Nothing -> case HashMap.lookup name (infoTable ^. Internal.infoAxioms) of
            Just ai -> case ai ^. Internal.axiomInfoDef . Internal.axiomBuiltin of
              Just Internal.BuiltinString -> IndString
              _ -> IndUser name
            Nothing -> IndUser name

    goTypeIden :: Internal.Iden -> Type
    goTypeIden = \case
      Internal.IdenFunction name -> mkIndType name []
      Internal.IdenConstructor name -> error ("unsupported type: constructor " <> Internal.ppTrace name)
      Internal.IdenVar name -> TyVar $ TypeVar name
      Internal.IdenAxiom name -> mkIndType name []
      Internal.IdenInductive name -> mkIndType name []

    goTypeApp :: Internal.Application -> Type
    goTypeApp app = mkIndType name params
      where
        (ind, args) = Internal.unfoldApplication app
        params = map goType (toList args)
        name = case ind of
          Internal.ExpressionIden (Internal.IdenFunction n) -> n
          Internal.ExpressionIden (Internal.IdenAxiom n) -> n
          Internal.ExpressionIden (Internal.IdenInductive n) -> n
          _ -> error ("unsupported type: " <> Internal.ppTrace app)

    goTypeFun :: Internal.Function -> Type
    goTypeFun Internal.Function {..} = case lty of
      Internal.ExpressionUniverse {} -> goType _functionRight
      _ ->
        TyFun $ FunType (goType lty) (goType _functionRight)
      where
        lty = _functionLeft ^. Internal.paramType

    goExpression :: Internal.Expression -> Expression
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
            _ -> name
        Nothing -> name

    goFunName :: Name -> Name
    goFunName name = name

    goIden :: Internal.Iden -> Expression
    goIden = \case
      Internal.IdenFunction name -> ExprIden (goFunName name)
      Internal.IdenConstructor name ->
        case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
          Just ctrInfo ->
            case ctrInfo ^. Internal.constructorInfoBuiltin of
              Just Internal.BuiltinNatZero -> ExprLiteral (LitNumeric 0)
              _ -> ExprIden (goConstrName name)
          Nothing -> ExprIden (goConstrName name)
      Internal.IdenVar name -> ExprIden name
      Internal.IdenAxiom name -> ExprIden name
      Internal.IdenInductive name -> ExprIden name

    goApplication :: Internal.Application -> Expression
    goApplication app@Internal.Application {..}
      | Just (PragmaIsabelleOperator {..}, arg1, arg2) <- getIsabelleOperator app =
          ExprBinop
            Binop
              { _binopOperator = defaultName _pragmaIsabelleOperatorName,
                _binopLeft = goExpression arg1,
                _binopRight = goExpression arg2,
                _binopFixity =
                  Fixity
                    { _fixityPrecedence = PrecNat (fromMaybe 0 _pragmaIsabelleOperatorPrec),
                      _fixityArity = OpBinary (fromMaybe AssocNone _pragmaIsabelleOperatorAssoc),
                      _fixityId = Nothing
                    }
              }
      | Just x <- getLiteralConversion app = ExprLiteral (LitNumeric x)
      | Just x <- getList app = ExprList (List x)
      | Just (x, y) <- getCons app = ExprCons (Cons x y)
      | Just (v, x, y) <- getIf app = ExprIf (If v x y)
      | otherwise =
          let l = goExpression _appLeft
              r = goExpression _appRight
           in ExprApp (Application l r)

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

    getLiteralConversion :: Internal.Application -> Maybe Integer
    getLiteralConversion app = case fn of
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

    getList :: Internal.Application -> Maybe [Expression]
    getList app = case fn of
      Internal.ExpressionIden (Internal.IdenConstructor name) ->
        case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
          Just ctrInfo ->
            case ctrInfo ^. Internal.constructorInfoBuiltin of
              Just Internal.BuiltinListNil -> Just []
              Just Internal.BuiltinListCons
                | (_ :| [arg1, Internal.ExpressionApplication app2]) <- args,
                  Just lst <- getList app2 ->
                    Just (goExpression arg1 : lst)
              _ -> Nothing
          Nothing -> Nothing
      _ -> Nothing
      where
        (fn, args) = Internal.unfoldApplication app

    getCons :: Internal.Application -> Maybe (Expression, Expression)
    getCons app = case fn of
      Internal.ExpressionIden (Internal.IdenConstructor name) ->
        case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
          Just ctrInfo ->
            case ctrInfo ^. Internal.constructorInfoBuiltin of
              Just Internal.BuiltinListCons
                | (_ :| [arg1, arg2]) <- args ->
                    Just (goExpression arg1, goExpression arg2)
              _ -> Nothing
          Nothing -> Nothing
      _ -> Nothing
      where
        (fn, args) = Internal.unfoldApplication app

    getIf :: Internal.Application -> Maybe (Expression, Expression, Expression)
    getIf app = case fn of
      Internal.ExpressionIden (Internal.IdenFunction name) ->
        case HashMap.lookup name (infoTable ^. Internal.infoFunctions) of
          Just funInfo ->
            case funInfo ^. Internal.functionInfoBuiltin of
              Just Internal.BuiltinBoolIf
                | (_ :| [val, br1, br2]) <- args ->
                    Just (goExpression val, goExpression br1, goExpression br2)
              _ -> Nothing
          Nothing -> Nothing
      _ -> Nothing
      where
        (fn, args) = Internal.unfoldApplication app

    goFunType :: Internal.Function -> Expression
    goFunType _ = ExprUndefined

    goLiteral :: Internal.LiteralLoc -> Expression
    goLiteral lit = case lit ^. withLocParam of
      Internal.LitString s -> ExprLiteral $ LitString s
      Internal.LitNumeric n -> ExprLiteral $ LitNumeric n
      Internal.LitInteger n -> ExprLiteral $ LitNumeric n
      Internal.LitNatural n -> ExprLiteral $ LitNumeric n

    goHole :: Internal.Hole -> Expression
    goHole _ = ExprUndefined

    goInstanceHole :: Internal.InstanceHole -> Expression
    goInstanceHole _ = ExprUndefined

    goLet :: Internal.Let -> Expression
    goLet Internal.Let {..} = go (concatMap toFunDefs (toList _letClauses))
      where
        toFunDefs :: Internal.LetClause -> [Internal.FunctionDef]
        toFunDefs = \case
          Internal.LetFunDef d -> [d]
          Internal.LetMutualBlock Internal.MutualBlockLet {..} -> toList _mutualLet

        go :: [Internal.FunctionDef] -> Expression
        go = \case
          d : defs' -> goFunDef d (go defs')
          [] -> goExpression _letExpression

        goFunDef :: Internal.FunctionDef -> Expression -> Expression
        goFunDef Internal.FunctionDef {..} expr =
          ExprLet
            Let
              { _letVar = _funDefName,
                _letValue = goExpression _funDefBody,
                _letBody = expr
              }

    goUniverse :: Internal.SmallUniverse -> Expression
    goUniverse _ = ExprUndefined

    goSimpleLambda :: Internal.SimpleLambda -> Expression
    goSimpleLambda Internal.SimpleLambda {..} =
      ExprLambda
        Lambda
          { _lambdaVar = _slambdaBinder ^. Internal.sbinderVar,
            _lambdaType = Just $ goType $ _slambdaBinder ^. Internal.sbinderType,
            _lambdaBody = goExpression _slambdaBody
          }

    -- TODO: properly unique names for lambda-bound variables
    goLambda :: Internal.Lambda -> Expression
    goLambda Internal.Lambda {..} = goLams vars
      where
        npats = length $ head _lambdaClauses ^. Internal.lambdaPatterns
        vars = map (\i -> defaultName ("X" <> show i)) [0 .. npats - 1]

        goLams :: [Name] -> Expression
        goLams = \case
          v : vs ->
            ExprLambda
              Lambda
                { _lambdaType = fmap goType _lambdaType,
                  _lambdaVar = v,
                  _lambdaBody = goLams vs
                }
          [] ->
            ExprCase
              Case
                { _caseValue = val,
                  _caseBranches = fmap goClause _lambdaClauses
                }
            where
              val =
                case vars of
                  [v] -> ExprIden v
                  _ ->
                    ExprTuple
                      Tuple
                        { _tupleComponents = nonEmpty' $ map ExprIden vars
                        }

        goClause :: Internal.LambdaClause -> CaseBranch
        goClause Internal.LambdaClause {..} =
          CaseBranch
            { _caseBranchPattern = pat,
              _caseBranchBody = goExpression _lambdaBody
            }
          where
            pat =
              case _lambdaPatterns of
                p :| [] -> goPatternArg p
                _ ->
                  PatTuple
                    Tuple
                      { _tupleComponents = fmap goPatternArg _lambdaPatterns
                      }

    goCase :: Internal.Case -> Expression
    goCase Internal.Case {..} =
      ExprCase
        Case
          { _caseValue = goExpression _caseExpression,
            _caseBranches = fmap goCaseBranch _caseBranches
          }

    goCaseBranch :: Internal.CaseBranch -> CaseBranch
    goCaseBranch Internal.CaseBranch {..} =
      CaseBranch
        { _caseBranchPattern = goPatternArg _caseBranchPattern,
          _caseBranchBody = goCaseBranchRhs _caseBranchRhs
        }

    goCaseBranchRhs :: Internal.CaseBranchRhs -> Expression
    goCaseBranchRhs = \case
      Internal.CaseBranchRhsExpression e -> goExpression e
      Internal.CaseBranchRhsIf {} -> error "unsupported: side conditions"

    -- TODO: named patterns (`_patternArgName`) are not handled properly
    goPatternArg :: Internal.PatternArg -> Pattern
    goPatternArg Internal.PatternArg {..} =
      goPattern _patternArgPattern

    goPattern :: Internal.Pattern -> Pattern
    goPattern = \case
      Internal.PatternVariable x -> PatVar x
      Internal.PatternConstructorApp x -> goPatternConstructorApp x
      Internal.PatternWildcardConstructor {} -> impossible

    goPatternConstructorApp :: Internal.ConstructorApp -> Pattern
    goPatternConstructorApp Internal.ConstructorApp {..}
      | Just lst <- getListPat _constrAppConstructor _constrAppParameters =
          PatList (List lst)
      | Just (x, y) <- getConsPat _constrAppConstructor _constrAppParameters =
          PatCons (Cons x y)
      | Just p <- getNatPat _constrAppConstructor _constrAppParameters =
          p
      | otherwise =
          PatConstrApp
            ConstrApp
              { _constrAppConstructor = goConstrName _constrAppConstructor,
                _constrAppArgs = map goPatternArg _constrAppParameters
              }

    -- This function cannot be simply merged with `getList` because in patterns
    -- the constructors don't get the type argument.
    getListPat :: Name -> [Internal.PatternArg] -> Maybe [Pattern]
    getListPat name args =
      case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
        Just funInfo ->
          case funInfo ^. Internal.constructorInfoBuiltin of
            Just Internal.BuiltinListNil -> Just []
            Just Internal.BuiltinListCons
              | [arg1, Internal.PatternArg {..}] <- args,
                Internal.PatternConstructorApp Internal.ConstructorApp {..} <- _patternArgPattern,
                Just lst <- getListPat _constrAppConstructor _constrAppParameters ->
                  Just (goPatternArg arg1 : lst)
            _ -> Nothing
        Nothing -> Nothing

    getConsPat :: Name -> [Internal.PatternArg] -> Maybe (Pattern, Pattern)
    getConsPat name args =
      case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
        Just funInfo ->
          case funInfo ^. Internal.constructorInfoBuiltin of
            Just Internal.BuiltinListCons
              | [arg1, arg2] <- args ->
                  Just (goPatternArg arg1, goPatternArg arg2)
            _ -> Nothing
        Nothing -> Nothing

    getNatPat :: Name -> [Internal.PatternArg] -> Maybe Pattern
    getNatPat name args =
      case HashMap.lookup name (infoTable ^. Internal.infoConstructors) of
        Just funInfo ->
          case funInfo ^. Internal.constructorInfoBuiltin of
            Just Internal.BuiltinNatZero
              | null args ->
                  Just PatZero
            Just Internal.BuiltinNatSuc
              | [arg] <- args ->
                  Just (PatConstrApp (ConstrApp (goConstrName name) [goPatternArg arg]))
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
