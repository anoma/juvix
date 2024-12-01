module Juvix.Compiler.Backend.Lean.Translation.FromTyped where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List.NonEmpty.Extra qualified as NonEmpty
import Data.Text qualified as Text
import Juvix.Compiler.Backend.Lean.Data.Result
import Juvix.Compiler.Backend.Lean.Language (Module(..), Command(..), Type(..), Expression(..), Pattern(..), Literal(..), Level(..), Name)
import Juvix.Compiler.Backend.Lean.Language qualified as Lean
import Juvix.Compiler.Internal.Data.InfoTable qualified as Internal
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.TypeChecking.Data.Context qualified as Internal
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Store.Extra
import Juvix.Compiler.Store.Language
import Juvix.Prelude

fromInternal ::
  forall r.
  (Members '[Error JuvixError, Reader EntryPoint, Reader ModuleTable, NameIdGen] r) =>
  Internal.InternalTypedResult ->
  Sem r Result
fromInternal res@Internal.InternalTypedResult {..} = do
  itab <- getInternalModuleTable <$> ask
  let md :: Internal.InternalModule
      md = _resultInternalModule
      itab' :: Internal.InternalModuleTable
      itab' = Internal.insertInternalModule itab md
      table :: Internal.InfoTable
      table = Internal.computeCombinedInfoTable itab'
      file = getLoc _resultModule ^. intervalFile
      comments = filter 
        (\c -> c ^. commentInterval . intervalFile == file)
        (allComments (Internal.getInternalTypedResultComments res))
  module' <- convertModule table _resultModule
  return $ Result
    { _resultModule = module',
      _resultModuleId = md ^. Internal.internalModuleId,
      _resultComments = comments
    }

defaultName :: Interval -> Text -> Name
defaultName loc n =
  Internal.Name
    { _nameText = n,
      _nameId = defaultId,
      _nameKind = Internal.KNameLocal,
      _nameKindPretty = Internal.KNameLocal,
      _namePretty = n,
      _nameLoc = loc,
      _nameFixity = Nothing
    }
  where
    defaultId = NameId
      { _nameIdUid = 0,
        _nameIdModuleId = defaultModuleId 
      }

makeFreshName :: (Member NameIdGen r) => Interval -> Text -> Sem r Name
makeFreshName loc base = do
  uid <- freshNameId
  let name = Internal.Name
        { _nameText = base,
          _nameId = uid,
          _nameKind = Internal.KNameLocal,
          _nameKindPretty = Internal.KNameLocal,
          _namePretty = base,
          _nameLoc = loc,
          _nameFixity = Nothing
        }
  return name

convertModule :: (Member NameIdGen r) => Internal.InfoTable -> Internal.Module -> Sem r Lean.Module
convertModule table Internal.Module {..} = do
  let imports = map (^. Internal.importModuleName) (_moduleBody ^. Internal.moduleImports)
  commands <- concat <$> mapM goMutualBlock (_moduleBody ^. Internal.moduleStatements)
  return $ Lean.Module
    { _modulePrelude = False,
      _moduleImports = [],
      _moduleCommands = 
        [Lean.ModImport i | i <- imports] ++  -- Fixed: wrap in ModImport
        [Lean.ModNamespace _moduleName commands]
    }
  where
    goMutualBlock :: (Member NameIdGen r) => Internal.MutualBlock -> Sem r [Command] 
    goMutualBlock Internal.MutualBlock {..} =
      mapM goMutualStatement (toList _mutualStatements)

    goMutualStatement :: (Member NameIdGen r) => Internal.MutualStatement -> Sem r Command
    goMutualStatement = \case
      Internal.StatementInductive x 
        | x ^. Internal.inductiveTrait -> return $ goTraitDef x
        | otherwise -> goInductiveDef x
      Internal.StatementFunction x
        | isInstanceDef x -> goInstanceDef x
        | otherwise -> goFunctionDef x
      Internal.StatementAxiom x -> return $ goAxiomDef x

    goTraitDef :: Internal.InductiveDef -> Command
    goTraitDef Internal.InductiveDef {..} =
      CmdClass Lean.Class
        { _className = _inductiveName,
          _classParams = map goInductiveParameter _inductiveParameters,
          _classFields = concatMap getTraitFields _inductiveConstructors
        }
      where
        getTraitFields cdef = 
          let (args, _) = Internal.unfoldFunType (cdef ^. Internal.inductiveConstructorType)
          in map (\param -> 
                   ( fromMaybe (defaultName (getLoc param) "_") (param ^. Internal.paramName),
                     goType (param ^. Internal.paramType)
                   )) args

    isInstanceDef :: Internal.FunctionDef -> Bool
    isInstanceDef f = case f ^. Internal.funDefIsInstanceCoercion of
      Just Internal.IsInstanceCoercionInstance -> True
      Just Internal.IsInstanceCoercionCoercion -> True
      Nothing -> False

    goInstanceDef :: (Member NameIdGen r) => Internal.FunctionDef -> Sem r Command
    goInstanceDef Internal.FunctionDef {..} = do
      value <- goExpression _funDefBody
      return $ CmdInstance Lean.Instance 
        { _instanceName = Just _funDefName,
          _instanceType = goType _funDefType,
          _instanceValue = value,
          _instancePriority = Nothing
        }

    goInductiveDef :: (Member NameIdGen r) =>  Internal.InductiveDef -> Sem r Command
    goInductiveDef Internal.InductiveDef {..}
      | length _inductiveConstructors == 1
          && head' _inductiveConstructors ^. Internal.inductiveConstructorIsRecord = do
          let tyargs = fst $ Internal.unfoldFunType $ head' _inductiveConstructors ^. Internal.inductiveConstructorType
          body <- goRecordConstructor tyargs
          return $ CmdDefinition Lean.Definition
            { _definitionName = _inductiveName,
              _definitionType = Just (mkIndType _inductiveName []), 
              _definitionAttrs = [],
              _definitionBody = body
            }
      | otherwise = 
          return $ CmdInductive Lean.Inductive
            { _inductiveName = _inductiveName,
              _inductiveParams = map goInductiveParameter _inductiveParameters,
              _inductiveType = goType _inductiveType,
              _inductiveCtors = map goConstructorDef _inductiveConstructors
            }

    goRecordConstructor :: (Member NameIdGen r) => [Internal.FunctionParameter] -> Sem r Expression
    goRecordConstructor params = do
      fields <- mapM goRecordField params
      return $ ExprStruct Lean.Structure
        { _structBase = Nothing,
          _structFields = fields
        }

    goRecordField :: (Member NameIdGen r) => Internal.FunctionParameter -> Sem r (Name, Expression)
    goRecordField Internal.FunctionParameter {..} = do
      expr <- goExpression _paramType
      return (fromMaybe (defaultName (getLoc _paramType) "_") _paramName, expr)

    goInductiveParameter :: Internal.InductiveParameter -> Lean.Binder
    goInductiveParameter Internal.InductiveParameter {..} =
      Lean.Binder
        { _binderName = _inductiveParamName,
          _binderType = Just (goType _inductiveParamType),
          _binderInfo = Lean.BinderDefault,
          _binderDefault = Nothing
        }

    goConstructorDef :: Internal.ConstructorDef -> (Name, Type)
    goConstructorDef Internal.ConstructorDef {..} =
      (_inductiveConstructorName, goType _inductiveConstructorType)

    goFunctionDef :: (Member NameIdGen r) => Internal.FunctionDef -> Sem r Command
    goFunctionDef Internal.FunctionDef {..} = do
      body <- goExpression _funDefBody
      return $ CmdDefinition Lean.Definition
        { _definitionName = _funDefName
        , _definitionType = Just (goType _funDefType)
        , _definitionBody = body
        , _definitionAttrs = []
        }

    goAxiomDef :: Internal.AxiomDef -> Command
    goAxiomDef Internal.AxiomDef {..} =
      CmdAxiom Lean.Axiom
        { _axiomName = _axiomName,
          _axiomType = goType _axiomType,
          _axiomAttrs = []
        }

    goExpression :: (Member NameIdGen r) => Internal.Expression -> Sem r Expression
    goExpression = \case
      Internal.ExpressionIden (Internal.IdenVar name) -> 
        return $ ExprConst name []
      Internal.ExpressionIden (Internal.IdenConstructor name) ->
        return $ ExprConst name []
      Internal.ExpressionApplication app -> goExpressionApp app
      Internal.ExpressionLambda lam -> 
        let clauses = lam ^. Internal.lambdaClauses
        in if isPatternLambda clauses
           then goPatternLambda clauses
           else goSimpleLambda (NonEmpty.head clauses)
        where
          isPatternLambda :: NonEmpty Internal.LambdaClause -> Bool
          isPatternLambda clauses = 
            length clauses > 1 || -- Multiple clauses always means pattern matching
            any hasPatternMatching (toList clauses)
            where
              hasPatternMatching :: Internal.LambdaClause -> Bool
              hasPatternMatching clause = 
                any hasNonVariablePattern 
                    (toList $ clause ^. Internal.lambdaPatterns)
              
              hasNonVariablePattern :: Internal.PatternArg -> Bool
              hasNonVariablePattern pat = case pat ^. Internal.patternArgPattern of
                Internal.PatternVariable _ -> False
                _ -> True  
      Internal.ExpressionLet l -> goExpressionLet l
      Internal.ExpressionCase c -> goExpressionMatch c
      Internal.ExpressionLiteral lit -> return $ goLiteral lit
      Internal.ExpressionUniverse _ -> return $ ExprSort LevelZero
      Internal.ExpressionFunction f -> 
        case f ^. Internal.functionLeft of
          Internal.FunctionParameter {_paramImplicit = imp} -> do
            body <- goExpression (f ^. Internal.functionRight)
            case imp of
              Internal.Implicit -> return $ ExprPi Lean.PiType
                { _piTypeBinder = (goBinder (f ^. Internal.functionLeft)) { Lean._binderInfo = Lean.BinderImplicit }
                , _piTypeBody = goType (f ^. Internal.functionRight)
                }
              Internal.ImplicitInstance -> return $ ExprPi Lean.PiType
                { _piTypeBinder = (goBinder (f ^. Internal.functionLeft)) { Lean._binderInfo = Lean.BinderInstImplicit }
                , _piTypeBody = goType (f ^. Internal.functionRight)
                }
              Internal.Explicit -> return $ ExprApp Lean.Application
                { _appLeft = ExprLambda Lean.Lambda { _lambdaBinder = goBinder (f ^. Internal.functionLeft), _lambdaBody = body }
                , _appRight = body
                }
      Internal.ExpressionHole loc -> 
        return $ ExprHole (getLoc loc)
      Internal.ExpressionInstanceHole ih ->
        return $ ExprHole (getLoc ih)
      Internal.ExpressionSimpleLambda slambda -> do
        let binder = slambda ^. Internal.slambdaBinder
            name = binder ^. Internal.sbinderVar
        expr <- do
          bodyExpr <- goExpression (slambda ^. Internal.slambdaBody)
          return $ ExprLambda Lean.Lambda
            { _lambdaBinder = Lean.Binder
                { _binderName = name
                , _binderType = Nothing
                , Lean._binderInfo = Lean.BinderDefault
                , _binderDefault = Nothing
                }
            , _lambdaBody = bodyExpr
            }
        return expr
      Internal.ExpressionIden (Internal.IdenAxiom name) -> 
          return $ ExprConst name []
      Internal.ExpressionIden (Internal.IdenInductive name) -> 
          return $ ExprConst name []
      Internal.ExpressionIden (Internal.IdenFunction name) -> 
          return $ ExprConst name []

    goExpressionApp :: (Member NameIdGen r) => Internal.Application -> Sem r Expression
    goExpressionApp Internal.Application {..} = do
      left <- goExpression _appLeft
      right <- goExpression _appRight
      return $ ExprApp Lean.Application
        { _appLeft = left,
          _appRight = right
        }

    -- | Convert a pattern lambda to a match expression
    goPatternLambda :: (Member NameIdGen r) => NonEmpty Internal.LambdaClause -> Sem r Expression
    goPatternLambda clauses = buildNestedLambdas 0 (NonEmpty.head clauses)
      where
        buildNestedLambdas :: (Member NameIdGen r) => Int -> Internal.LambdaClause -> Sem r Expression
        buildNestedLambdas pos Internal.LambdaClause{..} = 
          case _lambdaPatterns of
            pat :| [] -> do
              body <- goExpression _lambdaBody
              processArg pos pat body
            pat :| rest -> do
              inner <- buildNestedLambdas (pos + 1) (Internal.LambdaClause (fromJust $ nonEmpty rest) _lambdaBody)
              processArg pos pat inner

        processArg :: (Member NameIdGen r) => Int -> Internal.PatternArg -> Expression -> Sem r Expression
        processArg pos pat body = case pat ^. Internal.patternArgPattern of
          Internal.PatternVariable name -> 
            -- Simple variable binding - wrap in regular lambda
            return $ ExprLambda Lean.Lambda
              { _lambdaBinder = Lean.Binder
                  { _binderName = name
                  , _binderType = Nothing
                  , _binderInfo = getBinderInfo pat
                  , _binderDefault = Nothing
                  }
              , _lambdaBody = body
              }
          _ -> do
            -- Pattern matching - build match expression
            branches <- mapM goCaseBranch
              [ Internal.CaseBranch
                  { _caseBranchPattern = clausePat
                  , _caseBranchRhs = Internal.CaseBranchRhsExpression body
                  }
              | Internal.LambdaClause pats body <- toList clauses
              , let clausePat = (toList pats) !! pos
              ]
            return $ ExprMatch Lean.Case
              { _caseValue = ExprVar 0
              , _caseBranches = nonEmpty' branches
              }

        getBinderInfo :: Internal.PatternArg -> Lean.BinderInfo  
        getBinderInfo pat = case pat ^. Internal.patternArgIsImplicit of
          Internal.Implicit -> Lean.BinderImplicit
          Internal.ImplicitInstance -> Lean.BinderInstImplicit  
          Internal.Explicit -> Lean.BinderDefault

    -- | Convert a simple lambda (non-pattern case)
    goSimpleLambda :: (Member NameIdGen r) => Internal.LambdaClause -> Sem r Expression
    goSimpleLambda (Internal.LambdaClause patterns body) = 
      foldr mkLambda <$> goExpression body <*> pure (toList patterns)
      where
        mkLambda :: Internal.PatternArg -> Expression -> Expression
        mkLambda pat inner = 
          ExprLambda Lean.Lambda
            { _lambdaBinder = Lean.Binder
                { _binderName = getBoundName pat
                , _binderType = Nothing
                , _binderInfo = getBinderInfo pat
                , _binderDefault = Nothing
                }
            , _lambdaBody = inner
            }

        getBoundName :: Internal.PatternArg -> Name
        getBoundName pat = case pat ^. Internal.patternArgPattern of
          Internal.PatternVariable name -> name
          _ -> error "Expected variable pattern in non-pattern lambda"

        getBinderInfo :: Internal.PatternArg -> Lean.BinderInfo
        getBinderInfo pat = case pat ^. Internal.patternArgIsImplicit of
          Internal.Implicit -> Lean.BinderImplicit
          Internal.ImplicitInstance -> Lean.BinderInstImplicit
          Internal.Explicit -> Lean.BinderDefault

    goExpressionLet :: (Member NameIdGen r) => Internal.Let -> Sem r Expression
    goExpressionLet Internal.Let {..} = do
      case head _letClauses of
        Internal.LetFunDef fun -> do
          expr <- do
            value <- goExpression (fun ^. Internal.funDefBody)
            body <- goExpression _letExpression
            return $ ExprLet Lean.Let
              { _letName = fun ^. Internal.funDefName,
                _letType = Just $ goType (fun ^. Internal.funDefType),
                _letValue = value,
                _letBody = body
              }
          return expr
        _ -> error "Unsupported let binding"

    goExpressionMatch :: (Member NameIdGen r) => Internal.Case -> Sem r Expression
    goExpressionMatch Internal.Case {..} = do
      value <- goExpression _caseExpression
      branches <- mapM goCaseBranch (toList _caseBranches)
      return $ ExprMatch Lean.Case
        { _caseValue = value,
          _caseBranches = nonEmpty' branches
        }

    goCaseBranch :: (Member NameIdGen r) => Internal.CaseBranch -> Sem r Lean.CaseBranch
    goCaseBranch Internal.CaseBranch {..} = do
      let pat = _caseBranchPattern ^. Internal.patternArgPattern  -- Extract the inner pattern
      translatedPat <- goPattern (getLoc _caseBranchPattern) pat
      body <- case _caseBranchRhs of
        Internal.CaseBranchRhsExpression e -> goExpression e
        Internal.CaseBranchRhsIf _ -> error "Side conditions not supported yet"
      return Lean.CaseBranch
        { _caseBranchPattern = translatedPat,
          _caseBranchBody = body
        }

    goPattern :: (Member NameIdGen r) =>  Interval -> Internal.Pattern -> Sem r Pattern
    goPattern loc = \case
      Internal.PatternVariable name ->
        return (PatVar name)
      Internal.PatternWildcardConstructor _ -> do
        name <- makeFreshName loc "_"
        return (PatVar name)
      Internal.PatternConstructorApp c -> 
        goPatternConstructor loc c

    goPatternConstructor :: (Member NameIdGen r) => Interval -> Internal.ConstructorApp -> Sem r Pattern
    goPatternConstructor loc Internal.ConstructorApp {..} = do
      (pats) <- foldM
        (\ps p -> do
           p' <- goPattern loc (p ^. Internal.patternArgPattern)
           return (ps ++ [p']))
        []
        _constrAppParameters
      return (PatCtor _constrAppConstructor pats)

    goLiteral :: Internal.LiteralLoc -> Expression
    goLiteral l = case l ^. Internal.withLocParam of
      Internal.LitNumeric n -> ExprLiteral (WithLoc (getLoc l) (LitNumeric n))
      Internal.LitInteger n -> ExprLiteral (WithLoc (getLoc l) (LitNumeric n))
      Internal.LitNatural n -> ExprLiteral (WithLoc (getLoc l) (LitNumeric n))
      Internal.LitString s -> ExprLiteral (WithLoc (getLoc l) (LitString s))

    goType :: Internal.Expression -> Type
    goType = \case
      Internal.ExpressionIden x -> goTypeIden x
      Internal.ExpressionApplication x -> goTypeApp x
      Internal.ExpressionFunction x -> goTypeFun x
      Internal.ExpressionUniverse _ -> TySort LevelZero
      _ -> error $ "unsupported type: " -- <> show e

    goTypeIden :: Internal.Iden -> Type
    goTypeIden = \case
      Internal.IdenFunction name -> TyVar (Lean.TypeVar name)
      Internal.IdenConstructor name -> TyVar (Lean.TypeVar name)
      Internal.IdenVar name -> TyVar (Lean.TypeVar name)
      Internal.IdenAxiom name -> TyVar (Lean.TypeVar name)
      Internal.IdenInductive name -> TyVar (Lean.TypeVar name)

    goTypeApp :: Internal.Application -> Type
    goTypeApp Internal.Application {..} =
      TyApp Lean.TypeApp
        { _typeAppHead = goType _appLeft,
          _typeAppArg = goType _appRight
        }

    goTypeFun :: Internal.Function -> Type
    goTypeFun Internal.Function {..} =
      case _functionLeft of
        Internal.FunctionParameter {_paramImplicit = Internal.Implicit} ->
          TyPi Lean.PiType
            { _piTypeBinder = goBinder _functionLeft,
              _piTypeBody = goType _functionRight
            }
        Internal.FunctionParameter {_paramImplicit = Internal.Explicit} ->
          TyFun Lean.FunType
            { _funTypeLeft = goType (_functionLeft ^. Internal.paramType),
              _funTypeRight = goType _functionRight
            }
        Internal.FunctionParameter {_paramImplicit = Internal.ImplicitInstance} ->
          TyPi Lean.PiType
            { _piTypeBinder = goBinder _functionLeft,
              _piTypeBody = goType _functionRight
            }

    goBinder :: Internal.FunctionParameter -> Lean.Binder
    goBinder Internal.FunctionParameter {..} =
      Lean.Binder
        { _binderName = fromMaybe (defaultName (getLoc _paramType) "_") _paramName,
          _binderType = Just (goType _paramType),
          _binderInfo = case _paramImplicit of
            Internal.Implicit -> Lean.BinderImplicit
            Internal.ImplicitInstance -> Lean.BinderInstImplicit
            Internal.Explicit -> Lean.BinderDefault,
          _binderDefault = Nothing
        }

    mkIndType :: Name -> [Type] -> Type
    mkIndType name params = case HashMap.lookup name (table ^. Internal.infoInductives) of
      Just ii -> case ii ^. Internal.inductiveInfoBuiltin of
        Just Internal.BuiltinBool -> TySort LevelZero
        Just Internal.BuiltinNat -> TySort LevelZero
        Just Internal.BuiltinInt -> TySort LevelZero
        Just Internal.BuiltinList -> foldr mkApp (TyVar (Lean.TypeVar name)) params
        Just Internal.BuiltinMaybe -> foldr mkApp (TyVar (Lean.TypeVar name)) params
        Just Internal.BuiltinPair -> foldr mkApp (TyVar (Lean.TypeVar name)) params
        _ -> foldr mkApp (TyVar (Lean.TypeVar name)) params
      Nothing -> foldr mkApp (TyVar (Lean.TypeVar name)) params
      where
        mkApp :: Type -> Type -> Type
        mkApp arg hd = TyApp (Lean.TypeApp
          { _typeAppHead = hd,
            _typeAppArg = arg
          })
