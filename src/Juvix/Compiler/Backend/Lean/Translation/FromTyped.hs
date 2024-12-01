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

data DeBruijnInfo = DeBruijnInfo
  { _binderVarName :: Name,
    _binderIndex :: Int
  }

data TranslationContext = TranslationContext
  { _ctxBinders :: [DeBruijnInfo],
    _ctxGlobalVars :: HashSet Name,
    _ctxUsedNames :: HashSet Text,
    _ctxBinderDepth :: Int
  }

makeLenses ''DeBruijnInfo
makeLenses ''TranslationContext

emptyContext :: TranslationContext
emptyContext = TranslationContext [] mempty mempty 0

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
      ctx = buildInitialContext _resultModule
      file = getLoc _resultModule ^. intervalFile
      comments = filter 
        (\c -> c ^. commentInterval . intervalFile == file)
        (allComments (Internal.getInternalTypedResultComments res))
  module' <- convertModule ctx table _resultModule
  return $ Result
    { _resultModule = module',
      _resultModuleId = md ^. Internal.internalModuleId,
      _resultComments = comments
    }
  where
    buildInitialContext :: Internal.Module -> TranslationContext
    buildInitialContext m =
      let globals = HashSet.fromList $ concatMap getNames (m ^. Internal.moduleBody . Internal.moduleStatements)
      in TranslationContext [] globals mempty 0
      where
        getNames :: Internal.MutualBlock -> [Name]
        getNames (Internal.MutualBlock stmts) = concatMap getName (toList stmts)
        
        getName :: Internal.MutualStatement -> [Name]
        getName = \case
          Internal.StatementInductive ind -> [ind ^. Internal.inductiveName]
          Internal.StatementFunction fun -> [fun ^. Internal.funDefName]
          Internal.StatementAxiom ax -> [ax ^. Internal.axiomName]

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

disambiguate :: TranslationContext -> Text -> Text
disambiguate ctx = disambiguate' . quote
  where
    disambiguate' :: Text -> Text
    disambiguate' name
      | name == "?" || name == "" || name == "_" = disambiguate' "X"
      | HashSet.member name (ctx ^. ctxUsedNames) = disambiguate' (prime name)
      | otherwise = name
    
    quote :: Text -> Text
    quote = Text.filter isValidNameChar . ensureStartsWithLetter
      where
        isValidNameChar c = isAlphaNum c || c == '_' || c == '\''
        ensureStartsWithLetter t = case Text.uncons t of
          Just (c, _) | isLetter c -> t
          _ -> "x_" <> t

makeFreshName :: (Member NameIdGen r) => TranslationContext -> Interval -> Text -> Sem r (Name, TranslationContext)
makeFreshName ctx loc base = do
  let freshText = disambiguate ctx base
  uid <- freshNameId
  let name = Internal.Name
        { _nameText = freshText,
          _nameId = uid,
          _nameKind = Internal.KNameLocal,
          _nameKindPretty = Internal.KNameLocal,
          _namePretty = freshText,
          _nameLoc = loc,
          _nameFixity = Nothing
        }
  let ctx' = over ctxUsedNames (HashSet.insert freshText) ctx
  return (name, ctx')

extendContext :: (Member NameIdGen r) => Interval -> Text -> TranslationContext -> Sem r (Name, TranslationContext)
extendContext loc base ctx = do
  (name, ctx') <- makeFreshName ctx loc base
  let binder = DeBruijnInfo
        { _binderVarName = name,
          _binderIndex = ctx ^. ctxBinderDepth
        }
  return
    ( name,
      ctx' & ctxBinders %~ (binder:)
           & ctxBinderDepth %~ (+ 1)
    )

withBinders :: (Member NameIdGen r) => [(Interval, Text)] -> TranslationContext -> (TranslationContext -> Sem r a) -> Sem r (a, TranslationContext)
withBinders binderSpecs ctx action = do
  (ctx', _) <- foldM
    (\(c, ns) (loc, base) -> do
       (n, c') <- extendContext loc base c
       return (c', ns ++ [n]))
    (ctx, [])
    binderSpecs
  result <- action ctx'
  let finalCtx = ctx & ctxUsedNames .~ (ctx' ^. ctxUsedNames)
  return (result, finalCtx)

convertModule :: (Member NameIdGen r) => TranslationContext -> Internal.InfoTable -> Internal.Module -> Sem r Lean.Module
convertModule initCtx table Internal.Module {..} = do
  let imports = map (^. Internal.importModuleName) (_moduleBody ^. Internal.moduleImports)
  commands <- concat <$> mapM (goMutualBlock initCtx) (_moduleBody ^. Internal.moduleStatements)
  return $ Lean.Module
    { _modulePrelude = False,
      _moduleImports = [],
      _moduleCommands = 
        [Lean.ModImport i | i <- imports] ++  -- Fixed: wrap in ModImport
        [Lean.ModNamespace _moduleName commands]
    }
  where
    goMutualBlock :: (Member NameIdGen r) => TranslationContext -> Internal.MutualBlock -> Sem r [Command] 
    goMutualBlock ctx Internal.MutualBlock {..} =
      mapM (goMutualStatement ctx) (toList _mutualStatements)

    goMutualStatement :: (Member NameIdGen r) => TranslationContext -> Internal.MutualStatement -> Sem r Command
    goMutualStatement ctx = \case
      Internal.StatementInductive x 
        | x ^. Internal.inductiveTrait -> return $ goTraitDef ctx x
        | otherwise -> goInductiveDef ctx x
      Internal.StatementFunction x
        | isInstanceDef x -> goInstanceDef ctx x
        | otherwise -> goFunctionDef ctx x
      Internal.StatementAxiom x -> return $ goAxiomDef ctx x

    goTraitDef :: TranslationContext -> Internal.InductiveDef -> Command
    goTraitDef ctx Internal.InductiveDef {..} =
      CmdClass Lean.Class
        { _className = _inductiveName,
          _classParams = map (goInductiveParameter ctx) _inductiveParameters,
          _classFields = concatMap getTraitFields _inductiveConstructors
        }
      where
        getTraitFields cdef = 
          let (args, _) = Internal.unfoldFunType (cdef ^. Internal.inductiveConstructorType)
          in map (\param -> 
                   ( fromMaybe (defaultName (getLoc param) "_") (param ^. Internal.paramName),
                     goType ctx (param ^. Internal.paramType)
                   )) args

    isInstanceDef :: Internal.FunctionDef -> Bool
    isInstanceDef f = case f ^. Internal.funDefIsInstanceCoercion of
      Just Internal.IsInstanceCoercionInstance -> True
      Just Internal.IsInstanceCoercionCoercion -> True
      Nothing -> False

    goInstanceDef :: (Member NameIdGen r) => TranslationContext -> Internal.FunctionDef -> Sem r Command
    goInstanceDef ctx Internal.FunctionDef {..} = do
      value <- goExpression ctx _funDefBody
      return $ CmdInstance Lean.Instance 
        { _instanceName = Just _funDefName,
          _instanceType = goType ctx _funDefType,
          _instanceValue = value,
          _instancePriority = Nothing
        }

    goInductiveDef :: (Member NameIdGen r) => TranslationContext -> Internal.InductiveDef -> Sem r Command
    goInductiveDef ctx Internal.InductiveDef {..}
      | length _inductiveConstructors == 1
          && head' _inductiveConstructors ^. Internal.inductiveConstructorIsRecord = do
          let tyargs = fst $ Internal.unfoldFunType $ head' _inductiveConstructors ^. Internal.inductiveConstructorType
          body <- goRecordConstructor ctx tyargs
          return $ CmdDefinition Lean.Definition
            { _definitionName = _inductiveName,
              _definitionType = Just (mkIndType _inductiveName []), 
              _definitionAttrs = [],
              _definitionBody = body
            }
      | otherwise = 
          return $ CmdInductive Lean.Inductive
            { _inductiveName = _inductiveName,
              _inductiveParams = map (goInductiveParameter ctx) _inductiveParameters,
              _inductiveType = goType ctx _inductiveType,
              _inductiveCtors = map (goConstructorDef ctx) _inductiveConstructors
            }

    goRecordConstructor :: (Member NameIdGen r) => TranslationContext -> [Internal.FunctionParameter] -> Sem r Expression
    goRecordConstructor ctx params = do
      fields <- mapM (goRecordField ctx) params
      return $ ExprStruct Lean.Structure
        { _structBase = Nothing,
          _structFields = fields
        }

    goRecordField :: (Member NameIdGen r) => TranslationContext -> Internal.FunctionParameter -> Sem r (Name, Expression)
    goRecordField ctx Internal.FunctionParameter {..} = do
      expr <- goExpression ctx _paramType
      return (fromMaybe (defaultName (getLoc _paramType) "_") _paramName, expr)

    goInductiveParameter :: TranslationContext -> Internal.InductiveParameter -> Lean.Binder
    goInductiveParameter ctx Internal.InductiveParameter {..} =
      Lean.Binder
        { _binderName = _inductiveParamName,
          _binderType = Just (goType ctx _inductiveParamType),
          _binderInfo = Lean.BinderDefault,
          _binderDefault = Nothing
        }

    goConstructorDef :: TranslationContext -> Internal.ConstructorDef -> (Name, Type)
    goConstructorDef ctx Internal.ConstructorDef {..} =
      (_inductiveConstructorName, goType ctx _inductiveConstructorType)

    goFunctionDef :: (Member NameIdGen r) => TranslationContext -> Internal.FunctionDef -> Sem r Command
    goFunctionDef ctx Internal.FunctionDef {..} = do
      body <- goExpression ctx _funDefBody
      return $ CmdDefinition Lean.Definition
        { _definitionName = _funDefName
        , _definitionType = Just (goType ctx _funDefType)
        , _definitionBody = body
        , _definitionAttrs = []
        }

    goAxiomDef :: TranslationContext -> Internal.AxiomDef -> Command
    goAxiomDef ctx Internal.AxiomDef {..} =
      CmdAxiom Lean.Axiom
        { _axiomName = _axiomName,
          _axiomType = goType ctx _axiomType,
          _axiomAttrs = []
        }

    goExpression :: (Member NameIdGen r) => TranslationContext -> Internal.Expression -> Sem r Expression
    goExpression ctx = \case
      Internal.ExpressionIden (Internal.IdenVar name) -> 
        return $ ExprConst name []
      Internal.ExpressionIden (Internal.IdenFunction name)
        | HashSet.member name (ctx ^. ctxGlobalVars) ->
            return $ ExprConst name []
      Internal.ExpressionIden (Internal.IdenConstructor name) ->
        return $ ExprConst name []
      Internal.ExpressionApplication app -> goExpressionApp ctx app
      Internal.ExpressionLambda lam -> 
        let clauses = lam ^. Internal.lambdaClauses
        in if isPatternLambda clauses
           then goPatternLambda ctx clauses
           else goSimpleLambda ctx (NonEmpty.head clauses)
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
      Internal.ExpressionLet l -> goExpressionLet ctx l
      Internal.ExpressionCase c -> goExpressionMatch ctx c
      Internal.ExpressionLiteral lit -> return $ goLiteral lit
      Internal.ExpressionUniverse _ -> return $ ExprSort LevelZero
      Internal.ExpressionFunction f -> 
        case f ^. Internal.functionLeft of
          Internal.FunctionParameter {_paramImplicit = imp} -> do
            body <- goExpression ctx (f ^. Internal.functionRight)
            case imp of
              Internal.Implicit -> return $ ExprPi Lean.PiType
                { _piTypeBinder = (goBinder (f ^. Internal.functionLeft)) { Lean._binderInfo = Lean.BinderImplicit }
                , _piTypeBody = goType ctx (f ^. Internal.functionRight)
                }
              Internal.ImplicitInstance -> return $ ExprPi Lean.PiType
                { _piTypeBinder = (goBinder (f ^. Internal.functionLeft)) { Lean._binderInfo = Lean.BinderInstImplicit }
                , _piTypeBody = goType ctx (f ^. Internal.functionRight)
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
        (expr, _) <- withBinders [(getLoc name, name ^. Internal.nameText)] ctx $ \newCtx -> do
          bodyExpr <- goExpression newCtx (slambda ^. Internal.slambdaBody)
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

    goExpressionApp :: (Member NameIdGen r) => TranslationContext -> Internal.Application -> Sem r Expression
    goExpressionApp ctx Internal.Application {..} = do
      left <- goExpression ctx _appLeft
      right <- goExpression ctx _appRight
      return $ ExprApp Lean.Application
        { _appLeft = left,
          _appRight = right
        }

    -- | Convert a pattern lambda to a match expression
    goPatternLambda :: (Member NameIdGen r) => TranslationContext -> NonEmpty Internal.LambdaClause -> Sem r Expression
    goPatternLambda ctx clauses = do
      branches <- mapM (goCaseBranch ctx) 
        [ Internal.CaseBranch
            { _caseBranchPattern = NonEmpty.head patterns
            , _caseBranchRhs = Internal.CaseBranchRhsExpression body
            }
        | Internal.LambdaClause patterns body <- toList clauses
        ]
      return $ ExprMatch Lean.Case
        { _caseValue = ExprVar 0
        , _caseBranches = nonEmpty' branches
        }

    -- | Convert a simple lambda (non-pattern case)
    goSimpleLambda :: (Member NameIdGen r) => TranslationContext -> Internal.LambdaClause -> Sem r Expression
    goSimpleLambda ctx (Internal.LambdaClause patterns body) = 
      foldr mkLambda <$> goExpression ctx body <*> pure (toList patterns)
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

    goExpressionLet :: (Member NameIdGen r) => TranslationContext -> Internal.Let -> Sem r Expression
    goExpressionLet ctx Internal.Let {..} = do
      case head _letClauses of
        Internal.LetFunDef fun -> do
          (expr, _) <- withBinders [(getLoc (fun ^. Internal.funDefName), fun ^. Internal.funDefName . Internal.nameText)] ctx $ \ctx' -> do
            value <- goExpression ctx (fun ^. Internal.funDefBody)
            body <- goExpression ctx' _letExpression
            return $ ExprLet Lean.Let
              { _letName = fun ^. Internal.funDefName,
                _letType = Just $ goType ctx (fun ^. Internal.funDefType),
                _letValue = value,
                _letBody = body
              }
          return expr
        _ -> error "Unsupported let binding"

    goExpressionMatch :: (Member NameIdGen r) => TranslationContext -> Internal.Case -> Sem r Expression
    goExpressionMatch ctx Internal.Case {..} = do
      value <- goExpression ctx _caseExpression
      branches <- mapM (goCaseBranch ctx) (toList _caseBranches)
      return $ ExprMatch Lean.Case
        { _caseValue = value,
          _caseBranches = nonEmpty' branches
        }

    goCaseBranch :: (Member NameIdGen r) => TranslationContext -> Internal.CaseBranch -> Sem r Lean.CaseBranch
    goCaseBranch ctx Internal.CaseBranch {..} = do
      let pat = _caseBranchPattern ^. Internal.patternArgPattern  -- Extract the inner pattern
      (translatedPat, ctx') <- goPattern ctx (getLoc _caseBranchPattern) pat
      body <- case _caseBranchRhs of
        Internal.CaseBranchRhsExpression e -> goExpression ctx' e
        Internal.CaseBranchRhsIf _ -> error "Side conditions not supported yet"
      return Lean.CaseBranch
        { _caseBranchPattern = translatedPat,
          _caseBranchBody = body
        }

    goPattern :: (Member NameIdGen r) => TranslationContext -> Interval -> Internal.Pattern -> Sem r (Pattern, TranslationContext)
    goPattern ctx loc = \case
      Internal.PatternVariable name ->
        return (PatVar name, ctx)
      Internal.PatternWildcardConstructor _ -> do
        (name, ctx') <- makeFreshName ctx loc "_"
        return (PatVar name, ctx')
      Internal.PatternConstructorApp c -> 
        goPatternConstructor ctx loc c

    goPatternConstructor :: (Member NameIdGen r) => TranslationContext -> Interval -> Internal.ConstructorApp -> Sem r (Pattern, TranslationContext)
    goPatternConstructor ctx loc Internal.ConstructorApp {..} = do
      (pats, ctx') <- foldM
        (\(ps, c) p -> do
           (p', c') <- goPattern c loc (p ^. Internal.patternArgPattern)
           return (ps ++ [p'], c'))
        ([], ctx)
        _constrAppParameters
      return (PatCtor _constrAppConstructor pats, ctx')

    goLiteral :: Internal.LiteralLoc -> Expression
    goLiteral l = case l ^. Internal.withLocParam of
      Internal.LitNumeric n -> ExprLiteral (WithLoc (getLoc l) (LitNumeric n))
      Internal.LitInteger n -> ExprLiteral (WithLoc (getLoc l) (LitNumeric n))
      Internal.LitNatural n -> ExprLiteral (WithLoc (getLoc l) (LitNumeric n))
      Internal.LitString s -> ExprLiteral (WithLoc (getLoc l) (LitString s))

    goType :: TranslationContext -> Internal.Expression -> Type
    goType ctx = \case
      Internal.ExpressionIden x -> goTypeIden x
      Internal.ExpressionApplication x -> goTypeApp ctx x
      Internal.ExpressionFunction x -> goTypeFun ctx x
      Internal.ExpressionUniverse _ -> TySort LevelZero
      _ -> error $ "unsupported type: " -- <> show e

    goTypeIden :: Internal.Iden -> Type
    goTypeIden = \case
      Internal.IdenFunction name -> TyVar (Lean.TypeVar name)
      Internal.IdenConstructor name -> TyVar (Lean.TypeVar name)
      Internal.IdenVar name -> TyVar (Lean.TypeVar name)
      Internal.IdenAxiom name -> TyVar (Lean.TypeVar name)
      Internal.IdenInductive name -> TyVar (Lean.TypeVar name)

    goTypeApp :: TranslationContext -> Internal.Application -> Type
    goTypeApp ctx Internal.Application {..} =
      TyApp Lean.TypeApp
        { _typeAppHead = goType ctx _appLeft,
          _typeAppArg = goType ctx _appRight
        }

    goTypeFun :: TranslationContext -> Internal.Function -> Type
    goTypeFun ctx Internal.Function {..} =
      case _functionLeft of
        Internal.FunctionParameter {_paramImplicit = Internal.Implicit} ->
          TyPi Lean.PiType
            { _piTypeBinder = goBinder _functionLeft,
              _piTypeBody = goType ctx _functionRight
            }
        Internal.FunctionParameter {_paramImplicit = Internal.Explicit} ->
          TyFun Lean.FunType
            { _funTypeLeft = goType ctx (_functionLeft ^. Internal.paramType),
              _funTypeRight = goType ctx _functionRight
            }
        Internal.FunctionParameter {_paramImplicit = Internal.ImplicitInstance} ->
          TyPi Lean.PiType
            { _piTypeBinder = goBinder _functionLeft,
              _piTypeBody = goType ctx _functionRight
            }

    goBinder :: Internal.FunctionParameter -> Lean.Binder
    goBinder Internal.FunctionParameter {..} =
      Lean.Binder
        { _binderName = fromMaybe (defaultName (getLoc _paramType) "_") _paramName,
          _binderType = Just (goType initCtx _paramType),
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
