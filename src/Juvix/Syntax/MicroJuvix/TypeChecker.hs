module Juvix.Syntax.MicroJuvix.TypeChecker
  ( module Juvix.Syntax.MicroJuvix.TypeChecker,
    module Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult,
    module Juvix.Syntax.MicroJuvix.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Internal.NameIdGen
import Juvix.Pipeline.EntryPoint qualified as E
import Juvix.Prelude hiding (fromEither)
import Juvix.Syntax.MicroJuvix.Error
import Juvix.Syntax.MicroJuvix.InfoTable
import Juvix.Syntax.MicroJuvix.Language.Extra
import Juvix.Syntax.MicroJuvix.LocalVars
import Juvix.Syntax.MicroJuvix.MicroJuvixArityResult
import Juvix.Syntax.MicroJuvix.MicroJuvixTypedResult
import Juvix.Syntax.MicroJuvix.TypeChecker.Inference

addIdens :: Member (State TypesTable) r => TypesTable -> Sem r ()
addIdens idens = modify (HashMap.union idens)

registerConstructor :: Members '[State TypesTable, Reader InfoTable] r => InductiveConstructorDef -> Sem r ()
registerConstructor ctr = do
  ty <- constructorType (ctr ^. inductiveConstructorName)
  modify (HashMap.insert (ctr ^. inductiveConstructorName) ty)

entryMicroJuvixTyped ::
  Members '[Error TypeCheckerError, NameIdGen] r =>
  MicroJuvixArityResult ->
  Sem r MicroJuvixTypedResult
entryMicroJuvixTyped res@MicroJuvixArityResult {..} = do
  (idens, r) <-
    runState (mempty :: TypesTable)
      . runReader entryPoint
      . runReader table
      $ mapM checkModule _resultModules
  return
    MicroJuvixTypedResult
      { _resultMicroJuvixArityResult = res,
        _resultModules = r,
        _resultIdenTypes = idens
      }
  where
    table :: InfoTable
    table = buildTable _resultModules

    entryPoint :: E.EntryPoint
    entryPoint = res ^. microJuvixArityResultEntryPoint

type NegativeTypeParameters = HashSet VarName

checkModule ::
  Members '[Reader E.EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable] r =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <-
    (evalState (mempty :: NegativeTypeParameters) . checkModuleBody) _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  forall r.
  Members '[Reader E.EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State NegativeTypeParameters] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader E.EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader E.EntryPoint, Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable, State NegativeTypeParameters] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive ind -> do
    checkInductiveDef ind
    mapM_ registerConstructor (ind ^. inductiveConstructors)
    ty <- inductiveType (ind ^. inductiveName)
    modify (HashMap.insert (ind ^. inductiveName) ty)
    return s
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom ax -> do
    modify (HashMap.insert (ax ^. axiomName) (ax ^. axiomType))
    return s

checkFunctionDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, State TypesTable] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  (funDef, idens) <- runInferenceDef $ do
    info <- lookupFunction _funDefName
    checkFunctionDefType _funDefType
    registerIden _funDefName _funDefType
    _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
    return
      FunctionDef
        { _funDefClauses = _funDefClauses',
          ..
        }
  addIdens idens
  return funDef

checkFunctionDefType ::
  forall r.
  Members '[Inference] r =>
  Expression ->
  Sem r ()
checkFunctionDefType = traverseOf_ (leafExpressions . _ExpressionHole) go
  where
    go :: Hole -> Sem r ()
    go h = freshMetavar h

checkExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Expression ->
  Sem r Expression
checkExpression expectedTy e = do
  e' <- inferExpression' e
  let inferredType = e' ^. typedType
  whenJustM (matchTypes expectedTy inferredType) (throw . err)
  return (e' ^. typedExpression)
  where
    err matchErr =
      ErrWrongType
        ( WrongType
            { _wrongTypeThing = Left e,
              _wrongTypeActual = matchErr ^. matchErrorRight,
              _wrongTypeExpected = matchErr ^. matchErrorLeft
            }
        )

checkFunctionParameter ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  FunctionParameter ->
  Sem r FunctionParameter
checkFunctionParameter (FunctionParameter mv i e) = do
  e' <- checkExpression (smallUniverse (getLoc e)) e
  return (FunctionParameter mv i e')

-------------------------------------------------------------------------------
-- Typechecking for data types
-------------------------------------------------------------------------------

type ErrorReference = Maybe Expression

type RecursionLimit = Int

checkInductiveDef ::
  Members '[Reader InfoTable, Error TypeCheckerError, State NegativeTypeParameters, Reader E.EntryPoint] r =>
  InductiveDef ->
  Sem r ()
checkInductiveDef ty@InductiveDef {..} =
  mapM_ (checkConstructorDef ty) _inductiveConstructors

checkConstructorDef ::
  Members
    '[ Reader E.EntryPoint,
       Reader InfoTable,
       Error TypeCheckerError,
       State NegativeTypeParameters
     ]
    r =>
  InductiveDef ->
  InductiveConstructorDef ->
  Sem r ()
checkConstructorDef ty ctor = do
  let indName = ty ^. inductiveName
      ctorName = ctor ^. inductiveConstructorName
  checkConstructorReturnType ty ctor
  numInductives <- HashMap.size <$> asks (^. infoInductives)
  noCheckPositivity <- asks (^. E.entryPointNoPositivity)
  unless (noCheckPositivity || ty ^. inductiveNoPositivity) $
    mapM_
      (checkStrictlyPositiveOccurrences ty ctorName indName numInductives Nothing)
      (ctor ^. inductiveConstructorParameters)

checkConstructorReturnType ::
  Members '[Reader InfoTable, Error TypeCheckerError] r =>
  InductiveDef ->
  InductiveConstructorDef ->
  Sem r ()
checkConstructorReturnType indType ctor = do
  let ctorName = ctor ^. inductiveConstructorName
      ctorReturnType = ctor ^. inductiveConstructorReturnType
      tyName = indType ^. inductiveName
      indParams = map (^. inductiveParamName) (indType ^. inductiveParameters)
      expectedReturnType =
        foldExplicitApplication
          (ExpressionIden (IdenInductive tyName))
          (map (ExpressionIden . IdenVar) indParams)
  when
    (ctorReturnType /= expectedReturnType)
    ( throw
        ( ErrWrongReturnType
            ( WrongReturnType
                { _wrongReturnTypeConstructorName = ctorName,
                  _wrongReturnTypeExpected = expectedReturnType,
                  _wrongReturnTypeActual = ctorReturnType
                }
            )
        )
    )

checkStrictlyPositiveOccurrences ::
  Members '[Reader InfoTable, Error TypeCheckerError, State NegativeTypeParameters] r =>
  InductiveDef ->
  ConstrName ->
  Name ->
  RecursionLimit ->
  ErrorReference ->
  Expression ->
  Sem r ()
checkStrictlyPositiveOccurrences ty ctorName name recLimit ref = helper False
  where
    indName :: Name
    indName = ty ^. inductiveName

    -- The following `helper` function determines if there is any negative
    -- occurence of the symbol `name` in the given expression. The `inside` flag
    -- used below indicates whether the current search is performed on the left
    -- of an inner arrow or not.

    helper ::
      Members '[Reader InfoTable, Error TypeCheckerError, State NegativeTypeParameters] r =>
      Bool ->
      Expression ->
      Sem r ()

    helper inside expr = case expr of
      ExpressionIden (IdenInductive ty') -> when (inside && name == ty') (strictlyPositivityError expr)
      ExpressionIden (IdenVar name') ->
        when inside $
          if
              | name == name' -> strictlyPositivityError expr
              | InductiveParameter name' `elem` ty ^. inductiveParameters -> modify (HashSet.insert name')
              | otherwise -> return ()
      ExpressionFunction (Function l r) -> helper True (l ^. paramType) >> helper False r
      ExpressionApplication tyApp -> do
        let (hdExpr, args) = unfoldApplication tyApp
        case hdExpr of
          ExpressionIden (IdenInductive ty') -> do
            when (inside && name == ty') (strictlyPositivityError expr)
            InductiveInfo indTy' <- lookupInductive ty'

            -- We now need to know whether `name` negatively occurs at `indTy'`
            -- or not. The way to know is by checking that the type ty'
            -- preserves the positivity condition, i.e., its type parameters
            -- are no negative.

            unless (indTy' ^. inductiveNoPositivity) $
              let paramsTy' = indTy' ^. inductiveParameters
                  go ::
                    Members '[Reader InfoTable, Error TypeCheckerError, State NegativeTypeParameters] r =>
                    InductiveDef ->
                    [(InductiveParameter, Expression)] ->
                    Sem r ()
                  go typ = \case
                    [] -> return ()
                    ((InductiveParameter pName, arg) : ps) -> do
                      negParms :: NegativeTypeParameters <- get
                      when (nameInExpression name arg) $ do
                        when (HashSet.member pName negParms) (strictlyPositivityError arg)
                        when (recLimit > 0) $
                          forM_ (typ ^. inductiveConstructors) $ \ctor' ->
                            mapM_
                              ( checkStrictlyPositiveOccurrences
                                  typ
                                  (ctor' ^. inductiveConstructorName)
                                  pName
                                  (recLimit - 1)
                                  (Just (fromMaybe arg ref))
                              )
                              (ctor' ^. inductiveConstructorParameters)
                              >> modify (HashSet.insert pName)
                      go ty ps
               in go indTy' (zip paramsTy' (toList args))
          _ -> return ()
      _ -> return ()

    strictlyPositivityError :: Members '[Error TypeCheckerError] r => Expression -> Sem r ()
    strictlyPositivityError expr = do
      let errLoc = fromMaybe expr ref
      throw
        ( ErrNoStrictPositivity $
            NoStrictPositivity
              { _noStrictPositivityType = indName,
                _noStrictPositivityConstructor = ctorName,
                _noStrictPositivityArgument = errLoc
              }
        )

-------------------------------------------------------------------------------

inferExpression ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Reader LocalVars, Inference] r =>
  Expression ->
  Sem r Expression
inferExpression = fmap (^. typedExpression) . inferExpression'

lookupVar :: Member (Reader LocalVars) r => Name -> Sem r Expression
lookupVar v = HashMap.lookupDefault impossible v <$> asks (^. localTypes)

checkFunctionClauseBody ::
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  LocalVars ->
  Expression ->
  Expression ->
  Sem r Expression
checkFunctionClauseBody locals expectedTy body =
  runReader locals (checkExpression expectedTy body)

checkFunctionClause ::
  forall r.
  Members '[Reader InfoTable, Error TypeCheckerError, NameIdGen, Inference] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause info FunctionClause {..} = do
  (locals, bodyTy) <- helper _clausePatterns clauseType
  let bodyTy' = substitutionE (localsToSubsE locals) bodyTy
  _clauseBody' <- checkFunctionClauseBody locals bodyTy' _clauseBody
  return
    FunctionClause
      { _clauseBody = _clauseBody',
        ..
      }
  where
    clauseType :: Expression
    clauseType = info ^. functionInfoDef . funDefType
    helper :: [PatternArg] -> Expression -> Sem r (LocalVars, Expression)
    helper pats ty = runState emptyLocalVars (go pats ty)
    go :: [PatternArg] -> Expression -> Sem (State LocalVars ': r) Expression
    go pats bodyTy = case pats of
      [] -> return bodyTy
      (p : ps) -> case bodyTy of
        ExpressionHole h -> do
          s <- queryMetavar h
          case s of
            Just h' -> go pats h'
            Nothing -> do
              freshMetavar h
              l <- ExpressionHole <$> freshHole (getLoc h)
              r <- ExpressionHole <$> freshHole (getLoc h)
              let fun = ExpressionFunction (Function (unnamedParameter l) r)
              whenJustM (matchTypes (ExpressionHole h) fun) impossible
              go pats fun
        _ -> case unfoldFunType bodyTy of
          ([], _) -> error "too many patterns"
          (par : pars, ret) -> do
            checkPattern _clauseName par p
            go ps (foldFunType pars ret)

typeOfArg :: FunctionParameter -> Expression
typeOfArg = (^. paramType)

matchIsImplicit :: Member (Error TypeCheckerError) r => IsImplicit -> PatternArg -> Sem r ()
matchIsImplicit expected actual =
  unless
    (expected == actual ^. patternArgIsImplicit)
    ( throw
        ( ErrArity
            ( ErrWrongPatternIsImplicit
                WrongPatternIsImplicit
                  { _wrongPatternIsImplicitExpected = expected,
                    _wrongPatternIsImplicitActual = actual
                  }
            )
        )
    )

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Error TypeCheckerError, State LocalVars, Inference, NameIdGen] r =>
  FunctionName ->
  FunctionParameter ->
  PatternArg ->
  Sem r ()
checkPattern funName = go
  where
    go :: FunctionParameter -> PatternArg -> Sem r ()
    go argTy patArg = do
      matchIsImplicit (argTy ^. paramImplicit) patArg
      tyVarMap <- fmap (ExpressionIden . IdenVar) . (^. localTyMap) <$> get
<<<<<<< HEAD
      ty <- normalizeType (substitutionE tyVarMap (typeOfArg argTy))
      let pat = patArg ^. patternArgPattern
=======
      ty <- normalizeType (substitutionE tyVarMap (argTy ^. paramType))
      let unbrace = \case
            PatternBraces b -> b
            x -> x
          pat = unbrace p
>>>>>>> 6bd835c (the branch is back to feet)
      case pat of
        PatternWildcard {} -> return ()
        PatternVariable v -> do
          modify (addType v ty)
          registerIden v ty
          case argTy ^. paramName of
            Just v' -> do
              modify (over localTyMap (HashMap.insert v' v))
            _ -> return ()
        PatternConstructorApp a -> do
          s <- checkSaturatedInductive ty
          info <- lookupConstructor (a ^. constrAppConstructor)
          let constrIndName = info ^. constructorInfoInductive
              constrName = a ^. constrAppConstructor
              err :: MatchError -> Sem r ()
              err m =
                throw
                  ( ErrWrongType
                      WrongType
                        { _wrongTypeThing = Right pat,
                          _wrongTypeExpected = m ^. matchErrorRight,
                          _wrongTypeActual = m ^. matchErrorLeft
                        }
                  )
          case s of
            Left hole -> do
              let indParams = info ^. constructorInfoInductiveParameters
                  numIndParams = length indParams
                  indName :: Iden
                  indName = IdenInductive (info ^. constructorInfoInductive)
                  loc = getLoc a
              paramHoles <- map ExpressionHole <$> replicateM numIndParams (freshHole loc)
              let patternTy = foldApplication (ExpressionIden indName) (zip (repeat Explicit) paramHoles)
              whenJustM
                (matchTypes patternTy (ExpressionHole hole))
                err
              let tyArgs = zipExact indParams paramHoles
              goConstr a tyArgs
            Right (ind, tyArgs) -> do
              when
                (ind /= constrIndName)
                ( throw
                    ( ErrWrongConstructorType
                        WrongConstructorType
                          { _wrongCtorTypeName = constrName,
                            _wrongCtorTypeExpected = ind,
                            _wrongCtorTypeActual = constrIndName,
                            _wrongCtorTypeFunName = funName
                          }
                    )
                )
              goConstr a tyArgs
      where
        goConstr :: ConstructorApp -> [(InductiveParameter, Expression)] -> Sem r ()
        goConstr app@(ConstructorApp c ps) ctx = do
          (_, psTys) <- constructorArgTypes <$> lookupConstructor c
          let psTys' = map (substituteIndParams ctx) psTys
              expectedNum = length psTys
          let w = map unnamedParameter psTys'
          when (expectedNum /= length ps) (throw (appErr app expectedNum))
          zipWithM_ go w ps
        appErr :: ConstructorApp -> Int -> TypeCheckerError
        appErr app expected =
          ErrArity
            ( ErrWrongConstructorAppLength
                ( WrongConstructorAppLength
                    { _wrongConstructorAppLength = app,
                      _wrongConstructorAppLengthExpected = expected
                    }
                )
            )
    checkSaturatedInductive :: Expression -> Sem r (Either Hole (InductiveName, [(InductiveParameter, Expression)]))
    checkSaturatedInductive ty = do
      i <- viewInductiveApp ty
      case i of
        Left hole -> return (Left hole)
        Right (ind, args) -> do
          params :: [InductiveParameter] <-
            (^. inductiveInfoDef . inductiveParameters)
              <$> lookupInductive ind
          let numArgs = length args
              numParams = length params
          when
            (numArgs < numParams)
            ( throw
                ( ErrTooFewArgumentsIndType
                    ( WrongNumberArgumentsIndType
                        { _wrongNumberArgumentsIndTypeActualType = ty,
                          _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                          _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                        }
                    )
                )
            )
          when
            (numArgs > numParams)
            ( throw
                ( ErrTooManyArgumentsIndType
                    ( WrongNumberArgumentsIndType
                        { _wrongNumberArgumentsIndTypeActualType = ty,
                          _wrongNumberArgumentsIndTypeActualNumArgs = numArgs,
                          _wrongNumberArgumentsIndTypeExpectedNumArgs = numParams
                        }
                    )
                )
            )
          return (Right (ind, zipExact params args))

freshHole :: Members '[Inference, NameIdGen] r => Interval -> Sem r Hole
freshHole l = do
  uid <- freshNameId
  let h = Hole uid l
  freshMetavar h
  return h

-- | Returns {A : Expression} → A
literalType :: Members '[NameIdGen] r => LiteralLoc -> Sem r TypedExpression
literalType l = do
  uid <- freshNameId
  let strA :: Text
      strA = "A"
      typeVar =
        Name
          { _nameText = strA,
            _nameId = uid,
            _namePretty = strA,
            _nameKind = KNameLocal,
            _nameLoc = getLoc l
          }
      param =
        FunctionParameter
          { _paramName = Just typeVar,
            _paramImplicit = Implicit,
            _paramType = smallUniverse (getLoc l)
          }
      type_ =
        ExpressionFunction
          Function
            { _functionLeft = param,
              _functionRight = ExpressionIden (IdenVar typeVar)
            }
  return
    TypedExpression
      { _typedType = type_,
        _typedExpression = ExpressionLiteral l
      }

inferExpression' ::
  forall r.
  Members '[Reader InfoTable, Reader LocalVars, Error TypeCheckerError, NameIdGen, Inference] r =>
  Expression ->
  Sem r TypedExpression
inferExpression' e = case e of
  ExpressionIden i -> inferIden i
  ExpressionApplication a -> inferApplication a
  ExpressionLiteral l -> goLiteral l
  ExpressionFunction f -> goExpressionFunction f
  ExpressionHole h -> inferHole h
  ExpressionUniverse u -> goUniverse u
  where
    inferHole :: Hole -> Sem r TypedExpression
    inferHole h = do
      freshMetavar h
      return
        TypedExpression
          { _typedExpression = ExpressionHole h,
            _typedType = ExpressionUniverse (SmallUniverse (getLoc h))
          }

    goUniverse :: SmallUniverse -> Sem r TypedExpression
    goUniverse u =
      return
        TypedExpression
          { _typedType = ExpressionUniverse u,
            _typedExpression = ExpressionUniverse u
          }
    goExpressionFunction :: Function -> Sem r TypedExpression
    goExpressionFunction (Function l r) = do
      let uni = smallUniverse (getLoc l)
      l' <- checkFunctionParameter l
      r' <- checkExpression uni r
      return (TypedExpression uni (ExpressionFunction (Function l' r')))
    goLiteral :: LiteralLoc -> Sem r TypedExpression
    goLiteral = literalType

    inferIden :: Iden -> Sem r TypedExpression
    inferIden i = case i of
      IdenFunction fun -> do
        info <- lookupFunction fun
        return (TypedExpression (info ^. functionInfoDef . funDefType) (ExpressionIden i))
      IdenConstructor c -> do
        ty <- constructorType c
        return (TypedExpression ty (ExpressionIden i))
      IdenVar v -> do
        ty <- lookupVar v
        return (TypedExpression ty (ExpressionIden i))
      IdenAxiom v -> do
        info <- lookupAxiom v
        return (TypedExpression (info ^. axiomInfoType) (ExpressionIden i))
      IdenInductive v -> do
        kind <- inductiveType v
        return (TypedExpression kind (ExpressionIden i))
    inferApplication :: Application -> Sem r TypedExpression
    inferApplication (Application l r i) = inferExpression' l >>= helper
      where
        helper :: TypedExpression -> Sem r TypedExpression
        helper l' = case l' ^. typedType of
          ExpressionFunction (Function (FunctionParameter mv _ funL) funR) -> do
            r' <- checkExpression funL r
            return
              TypedExpression
                { _typedExpression =
                    ExpressionApplication
                      Application
                        { _appLeft = l' ^. typedExpression,
                          _appRight = r,
                          _appImplicit = i
                        },
                  _typedType = substitutionApp (mv, r') funR
                }
          -- When we have have an application with a hole on the left: '_@1 x'
          -- We assume that it is a type application and thus 'x' must be a type.
          -- Not sure if this is always desirable.
          ExpressionHole h -> do
            q <- queryMetavar h
            case q of
              Just ty -> helper (set typedType ty l')
              Nothing -> do
                r' <- checkExpression (smallUniverse (getLoc h)) r
                h' <- freshHole (getLoc h)
                let fun = Function (unnamedParameter r') (ExpressionHole h')
                whenJustM (matchTypes (ExpressionHole h) (ExpressionFunction fun)) impossible
                return
                  TypedExpression
                    { _typedType = ExpressionHole h',
                      _typedExpression =
                        ExpressionApplication
                          Application
                            { _appLeft = l' ^. typedExpression,
                              _appRight = r',
                              _appImplicit = i
                            }
                    }
          _ -> throw tyErr
            where
              tyErr :: TypeCheckerError
              tyErr =
                ErrExpectedFunctionType
                  ( ExpectedFunctionType
                      { _expectedFunctionTypeExpression = e,
                        _expectedFunctionTypeApp = l,
                        _expectedFunctionTypeType = l' ^. typedType
                      }
                  )

viewInductiveApp ::
  Members '[Error TypeCheckerError, Inference] r =>
  Expression ->
  Sem r (Either Hole (InductiveName, [Expression]))
viewInductiveApp ty = case t of
  ExpressionIden (IdenInductive n) -> return (Right (n, as))
  ExpressionHole h -> do
    r <- queryMetavar h
    case r of
      Just h' -> viewInductiveApp h'
      Nothing -> return (Left h)
  _ -> throw (ErrImpracticalPatternMatching (ImpracticalPatternMatching ty))
  where
    (t, as) = viewTypeApp ty

viewTypeApp :: Expression -> (Expression, [Expression])
viewTypeApp t = case t of
  ExpressionApplication (Application l r _) ->
    second (`snoc` r) (viewTypeApp l)
  _ -> (t, [])
