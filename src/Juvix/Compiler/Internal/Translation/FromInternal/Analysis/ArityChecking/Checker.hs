module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
  )
where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Pretty
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.LocalVars
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

checkModule ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction b -> StatementFunction <$> checkMutualBlock b
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementInductive d -> StatementInductive <$> checkInductive d
  StatementAxiom a -> StatementAxiom <$> checkAxiom a
  StatementModule m -> StatementModule <$> checkModule m

checkInductive :: forall r. (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => InductiveDef -> Sem r InductiveDef
checkInductive d = do
  let _inductiveName = d ^. inductiveName
      _inductiveBuiltin = d ^. inductiveBuiltin
      _inductivePositive = d ^. inductivePositive
  (localVars, _inductiveParameters) <- checkParameters (d ^. inductiveParameters)
  _inductiveExamples <- runReader localVars (mapM checkExample (d ^. inductiveExamples))
  _inductiveConstructors <- runReader localVars (mapM checkConstructor (d ^. inductiveConstructors))
  return InductiveDef {..}
  where
    checkParameters :: [InductiveParameter] -> Sem r (LocalVars, [InductiveParameter])
    checkParameters = runState emptyLocalVars . mapM checkParam
      where
        checkParam :: InductiveParameter -> Sem (State LocalVars ': r) InductiveParameter
        checkParam = return

checkConstructor :: (Members '[Reader LocalVars, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => InductiveConstructorDef -> Sem r InductiveConstructorDef
checkConstructor c = do
  let _inductiveConstructorName = c ^. inductiveConstructorName
  _inductiveConstructorParameters <- mapM checkType (c ^. inductiveConstructorParameters)
  _inductiveConstructorExamples <- mapM checkExample (c ^. inductiveConstructorExamples)
  _inductiveConstructorReturnType <- checkType (c ^. inductiveConstructorReturnType)
  return InductiveConstructorDef {..}

-- | check the arity of some ty : Type
checkType :: (Members '[Reader LocalVars, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => Expression -> Sem r Expression
checkType = checkExpression ArityUnit

checkAxiom :: (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => AxiomDef -> Sem r AxiomDef
checkAxiom a = do
  let _axiomName = a ^. axiomName
      _axiomBuiltin = a ^. axiomBuiltin
  _axiomType <- withEmptyLocalVars (checkType (a ^. axiomType))
  return AxiomDef {..}

checkMutualBlock ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkMutualBlock (MutualBlock funs) = MutualBlock <$> mapM checkFunctionDef funs

checkFunctionDef ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  let arity = typeArity _funDefType
  _funDefClauses' <- mapM (checkFunctionClause arity) _funDefClauses
  _funDefExamples' <- withEmptyLocalVars (mapM checkExample _funDefExamples)
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        _funDefExamples = _funDefExamples',
        ..
      }

checkFunctionClause ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  Arity ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause ari cl = do
  hint <- guessArity (cl ^. clauseBody)
  (patterns', locals, bodyAri) <- checkLhs loc hint ari (cl ^. clausePatterns)
  body' <- runReader locals (checkExpression bodyAri (cl ^. clauseBody))
  return
    FunctionClause
      { _clauseName = cl ^. clauseName,
        _clausePatterns = patterns',
        _clauseBody = body'
      }
  where
    name = cl ^. clauseName
    loc = getLoc name

simplelambda :: a
simplelambda = error "simple lambda expressions are not supported by the arity checker"

withEmptyLocalVars :: Sem (Reader LocalVars : r) a -> Sem r a
withEmptyLocalVars = runReader emptyLocalVars

arityLet :: (Members '[Reader InfoTable] r) => Let -> Sem r Arity
arityLet l = guessArity (l ^. letExpression)

inferReplExpression :: Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r => Expression -> Sem r Expression
inferReplExpression e = do
  ari <- guessArity e
  withEmptyLocalVars (checkExpression ari e)

guessArity ::
  forall r.
  (Members '[Reader InfoTable] r) =>
  Expression ->
  Sem r Arity
guessArity = \case
  ExpressionHole {} -> return ArityUnknown
  ExpressionFunction {} -> return ArityUnit
  ExpressionLiteral {} -> return arityLiteral
  ExpressionApplication a -> appHelper a
  ExpressionIden i -> idenHelper i
  ExpressionUniverse {} -> return arityUniverse
  ExpressionSimpleLambda {} -> simplelambda
  ExpressionLambda l -> return (arityLambda l)
  ExpressionLet l -> arityLet l
  ExpressionCase l -> arityCase l
  where
    idenHelper :: Iden -> Sem r Arity
    idenHelper i = case i of
      IdenVar {} -> return ArityUnknown
      _ -> withEmptyLocalVars (idenArity i)

    appHelper :: Application -> Sem r Arity
    appHelper a = do
      f' <- arif
      let u = unfoldArity' f'
      return $ case refine args (u ^. ufoldArityParams) of
        Nothing -> ArityUnknown
        Just a' -> foldArity (set ufoldArityParams a' u)
      where
        (f, args) = second (map fst . toList) (unfoldApplication' a)

        refine :: [IsImplicit] -> [ArityParameter] -> Maybe [ArityParameter]
        refine as ps = case (as, ps) of
          (Explicit : as', ParamExplicit {} : ps') -> refine as' ps'
          (Implicit : as', ParamImplicit {} : ps') -> refine as' ps'
          (as'@(Explicit : _), ParamImplicit {} : ps') -> refine as' ps'
          (Implicit : _, ParamExplicit {} : _) -> Nothing
          ([], ps') -> Just ps'
          (_ : _, []) -> Nothing

        arif :: Sem r Arity
        arif = guessArity f

arityLiteral :: Arity
arityLiteral = ArityUnit

arityUniverse :: Arity
arityUniverse = ArityUnit

-- | All branches should have the same arity. If they are all the same, we
-- return that, otherwise we return ArityUnknown. Probably something better can
-- be done.
arityCase :: Members '[Reader InfoTable] r => Case -> Sem r Arity
arityCase c = do
  aris <- mapM (guessArity . (^. caseBranchExpression)) (c ^. caseBranches)
  return
    if
        | allSame aris -> head aris
        | otherwise -> ArityUnknown

-- | Lambdas can only have explicit arguments. Since we do not have dependent
-- types, it is ok to (partially) infer the arity of the lambda from the clause
-- with the most patterns.
arityLambda :: Lambda -> Arity
arityLambda l =
  foldArity
    UnfoldedArity
      { _ufoldArityParams = replicate (maximum1 (fmap numPatterns (l ^. lambdaClauses))) (ParamExplicit ArityUnknown),
        _ufoldArityRest = ArityRestUnknown
      }
  where
    numPatterns :: LambdaClause -> Int
    numPatterns (LambdaClause ps _) = length ps

checkLhs ::
  forall r.
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  Interval ->
  Arity ->
  Arity ->
  [PatternArg] ->
  Sem r ([PatternArg], LocalVars, Arity)
checkLhs loc guessedBody ariSignature pats = do
  (locals, (pats', bodyAri)) <- runState emptyLocalVars (goLhs ariSignature pats)
  return (pats', locals, bodyAri)
  where
    -- returns the expanded patterns and the rest of the Arity (the arity of the
    -- body once all the patterns have been processed).
    -- Does not insert holes greedily. I.e. implicit wildcards are only inserted
    -- between explicit parameters already in the pattern.
    goLhs :: Arity -> [PatternArg] -> Sem (State LocalVars ': r) ([PatternArg], Arity)
    goLhs a = \case
      [] -> case tailHelper a of
        Nothing -> return ([], a)
        Just tailUnderscores -> do
          wildcard <- genWildcard
          let a' = foldArity (over ufoldArityParams (drop tailUnderscores) (unfoldArity' a))
          return (replicate tailUnderscores wildcard, a')
      lhs@(p : ps) -> case a of
        ArityUnit ->
          throw
            ( ErrLhsTooManyPatterns
                LhsTooManyPatterns
                  { _lhsTooManyPatternsRemaining = p :| ps
                  }
            )
        ArityUnknown -> do
          p' <- checkPattern ArityUnknown p
          first (p' :) <$> goLhs ArityUnknown ps
        ArityFunction (FunctionArity l r) ->
          case (p ^. patternArgIsImplicit, l) of
            (Implicit, ParamImplicit) -> do
              b' <- checkPattern (arityParameter l) p
              first (b' :) <$> goLhs r ps
            (Implicit, ParamExplicit {}) ->
              throw
                ( ErrWrongPatternIsImplicit
                    WrongPatternIsImplicit
                      { _wrongPatternIsImplicitExpected = Explicit,
                        _wrongPatternIsImplicitActual = p
                      }
                )
            (Explicit, ParamImplicit) -> do
              wildcard <- genWildcard
              first (wildcard :) <$> goLhs r lhs
            (Explicit, ParamExplicit pa) -> do
              p' <- checkPattern pa p
              first (p' :) <$> goLhs r ps
      where
        genWildcard :: forall r'. (Members '[NameIdGen] r') => Sem r' PatternArg
        genWildcard = do
          var <- varFromWildcard (Wildcard loc)
          return (PatternArg Implicit Nothing (PatternVariable var))

    -- This is an heuristic and it can have an undesired result.
    -- Sometimes the outcome may even be confusing.
    tailHelper :: Arity -> Maybe Int
    tailHelper a
      | 0 < pref = Just pref
      | otherwise = Nothing
      where
        pref :: Int
        pref = aI - targetI
        preceedingImplicits :: Arity -> Int
        preceedingImplicits = length . takeWhile isParamImplicit . unfoldArity
          where
            isParamImplicit :: ArityParameter -> Bool
            isParamImplicit = \case
              ParamExplicit {} -> False
              ParamImplicit -> True
        aI :: Int
        aI = preceedingImplicits a
        targetI :: Int
        targetI = preceedingImplicits guessedBody

checkPattern ::
  forall r.
  (Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars] r) =>
  Arity ->
  PatternArg ->
  Sem r PatternArg
checkPattern ari = traverseOf (patternArgName . each) nameAri >=> traverseOf patternArgPattern patternAri
  where
    nameAri :: VarName -> Sem r VarName
    nameAri n = addArity n ari $> n
    patternAri :: Pattern -> Sem r Pattern
    patternAri = \case
      PatternVariable v -> addArity v ari $> PatternVariable v
      PatternConstructorApp c -> case ari of
        ArityUnit -> PatternConstructorApp <$> checkConstructorApp c
        ArityUnknown -> PatternConstructorApp <$> checkConstructorApp c
        ArityFunction {} ->
          throw
            ( ErrPatternFunction
                PatternFunction
                  { _patternFunction = c
                  }
            )

checkConstructorApp ::
  forall r.
  (Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars] r) =>
  ConstructorApp ->
  Sem r ConstructorApp
checkConstructorApp ca = do
  let c = ca ^. constrAppConstructor
  args <- (^. constructorInfoArgs) <$> lookupConstructor c
  let arities = map typeArity args
      n = length arities
      ps = ca ^. constrAppParameters
      lps = length ps
  when
    (n /= lps)
    ( throw
        ( ErrWrongConstructorAppLength
            WrongConstructorAppLength
              { _wrongConstructorAppLength = ca,
                _wrongConstructorAppLengthExpected = n
              }
        )
    )
  ps' <- zipWithM checkPattern arities ps
  return (ConstructorApp c ps' Nothing)

checkCase ::
  forall r.
  Members '[Error ArityCheckerError, Reader LocalVars, Reader InfoTable, NameIdGen] r =>
  Arity ->
  Case ->
  Sem r Case
checkCase ari l = do
  _caseBranches <- mapM checkCaseBranch (l ^. caseBranches)
  _caseExpression <- checkExpression ArityUnit (l ^. caseExpression)
  let _caseParens = l ^. caseParens
      _caseExpressionType :: Maybe Expression = Nothing
      _caseExpressionWholeType :: Maybe Expression = Nothing
  return Case {..}
  where
    checkCaseBranch :: CaseBranch -> Sem r CaseBranch
    checkCaseBranch = traverseOf caseBranchExpression (checkExpression ari)

checkLet ::
  forall r.
  (Members '[Error ArityCheckerError, Reader LocalVars, Reader InfoTable, NameIdGen] r) =>
  Arity ->
  Let ->
  Sem r Let
checkLet ari l = do
  _letClauses <- mapM checkLetClause (l ^. letClauses)
  _letExpression <- checkExpression ari (l ^. letExpression)
  return Let {..}
  where
    checkLetClause :: LetClause -> Sem r LetClause
    checkLetClause = \case
      LetFunDef f -> LetFunDef <$> checkFunctionDef f
      LetMutualBlock f -> LetMutualBlock <$> checkMutualBlock f

checkLambda ::
  forall r.
  (Members '[Error ArityCheckerError, Reader LocalVars, Reader InfoTable, NameIdGen] r) =>
  Arity ->
  Lambda ->
  Sem r Lambda
checkLambda ari l = do
  let _lambdaType = l ^. lambdaType
  _lambdaClauses <- mapM goClause (l ^. lambdaClauses)
  return Lambda {..}
  where
    goClause :: LambdaClause -> Sem r LambdaClause
    goClause (LambdaClause ps b) = do
      locals <- ask
      let uari@UnfoldedArity {..} = unfoldArity' ari
      (locals', (ps', rest)) <- runState locals (helper _ufoldArityRest (toList ps) _ufoldArityParams)
      let ariBody = foldArity (set ufoldArityParams rest uari)
      b' <- local (const locals') (checkExpression ariBody b)
      return (LambdaClause (fromJust (nonEmpty ps')) b')
      where
        -- returns the adjusted patterns and the not consumed arity
        helper :: ArityRest -> [PatternArg] -> [ArityParameter] -> Sem (State LocalVars ': r) ([PatternArg], [ArityParameter])
        helper rest = go
          where
            go :: [PatternArg] -> [ArityParameter] -> Sem (State LocalVars ': r) ([PatternArg], [ArityParameter])
            go pats as =
              case (pats, as) of
                ([], _) -> return ([], as)
                (p : ps', ParamExplicit paramAri : as') -> do
                  p' <- checkPattern paramAri p
                  first (p' :) <$> go ps' as'
                (_ : _, ParamImplicit {} : _) ->
                  -- The lambda is expected to have an implicit argument but it cannot have one.
                  -- TODO. think what to do in this case
                  return (pats, as)
                (_ : _, []) -> case rest of
                  ArityRestUnit -> error ("too many patterns in lambda: " <> ppTrace l <> "\n" <> prettyText ari)
                  ArityRestUnknown -> return (pats, [])

idenArity :: (Members '[Reader LocalVars, Reader InfoTable] r) => Iden -> Sem r Arity
idenArity = \case
  IdenVar v -> getLocalArity v
  IdenInductive i -> typeArity <$> inductiveType i
  IdenFunction f -> typeArity . (^. functionInfoDef . funDefType) <$> lookupFunction f
  IdenConstructor c -> typeArity <$> constructorType c
  IdenAxiom a -> typeArity . (^. axiomInfoType) <$> lookupAxiom a

-- | let x be some expression of type T. The argument of this function is T and it returns
-- the arity of x. In other words, given (T : Type), it returns the arity of the elements of T.
typeArity :: Expression -> Arity
typeArity = go
  where
    go :: Expression -> Arity
    go = \case
      ExpressionIden i -> goIden i
      ExpressionApplication {} -> ArityUnit
      ExpressionLiteral {} -> ArityUnknown
      ExpressionFunction f -> ArityFunction (goFun f)
      ExpressionHole {} -> ArityUnknown
      ExpressionLambda {} -> ArityUnknown
      ExpressionCase {} -> ArityUnknown
      ExpressionUniverse {} -> ArityUnit
      ExpressionSimpleLambda {} -> simplelambda
      ExpressionLet l -> goLet l

    goLet :: Let -> Arity
    goLet l = typeArity (l ^. letExpression)

    goIden :: Iden -> Arity
    goIden = \case
      IdenVar {} -> ArityUnknown
      IdenInductive {} -> ArityUnit
      IdenFunction {} -> ArityUnknown -- we need normalization to determine the arity
      IdenConstructor {} -> ArityUnknown -- will be a type error
      IdenAxiom {} -> ArityUnknown

    goParam :: FunctionParameter -> ArityParameter
    goParam (FunctionParameter _ i e) = case i of
      Implicit -> ParamImplicit
      Explicit -> ParamExplicit (go e)

    goFun :: Function -> FunctionArity
    goFun (Function l r) =
      let l' = goParam l
          r' = go r
       in FunctionArity
            { _functionArityLeft = l',
              _functionArityRight = r'
            }

checkExample ::
  forall r.
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, Reader LocalVars] r) =>
  Example ->
  Sem r Example
checkExample = traverseOf exampleExpression (checkExpression ArityUnknown)

checkExpression ::
  forall r.
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, Reader LocalVars] r) =>
  Arity ->
  Expression ->
  Sem r Expression
checkExpression hintArity expr = case expr of
  ExpressionIden {} -> goApp expr []
  ExpressionApplication a -> uncurry goApp $ second toList (unfoldApplication' a)
  ExpressionLiteral {} -> appHelper expr []
  ExpressionFunction {} -> return expr
  ExpressionUniverse {} -> return expr
  ExpressionHole {} -> return expr
  ExpressionSimpleLambda {} -> simplelambda
  ExpressionLambda l -> ExpressionLambda <$> checkLambda hintArity l
  ExpressionLet l -> ExpressionLet <$> checkLet hintArity l
  ExpressionCase l -> ExpressionCase <$> checkCase hintArity l
  where
    goApp :: Expression -> [(IsImplicit, Expression)] -> Sem r Expression
    goApp f args = do
      case f of
        ExpressionIden (IdenAxiom n) -> do
          blt <- getAxiomBuiltinInfo n
          case blt of
            Just BuiltinIOSequence -> goBuiltinApp n 0 2 f args
            Just BuiltinTrace -> goBuiltinApp n 1 1 f args
            _ -> appHelper f args
        ExpressionIden (IdenFunction n) -> do
          blt <- getFunctionBuiltinInfo n
          case blt of
            Just BuiltinBoolIf -> goBuiltinApp n 1 3 f args
            Just BuiltinBoolOr -> goBuiltinApp n 0 2 f args
            Just BuiltinBoolAnd -> goBuiltinApp n 0 2 f args
            Just BuiltinSeq -> goBuiltinApp n 2 2 f args
            _ -> appHelper f args
        _ -> appHelper f args

    goBuiltinApp :: Name -> Int -> Int -> Expression -> [(IsImplicit, Expression)] -> Sem r Expression
    goBuiltinApp n implArgsNum argsNum f args = do
      args' <- goImplArgs implArgsNum args
      if
          | length args' >= argsNum -> appHelper f args
          | otherwise ->
              throw $
                ErrBuiltinNotFullyApplied
                  BuiltinNotFullyApplied
                    { _builtinNotFullyAppliedName = n,
                      _builtinNotFullyAplliedExpectedArgsNum = argsNum
                    }
      where
        goImplArgs :: Int -> [(IsImplicit, Expression)] -> Sem r [(IsImplicit, Expression)]
        goImplArgs 0 as = return as
        goImplArgs k ((Implicit, _) : as) = goImplArgs (k - 1) as
        goImplArgs _ as = return as

    appHelper :: Expression -> [(IsImplicit, Expression)] -> Sem r Expression
    appHelper fun0 args = do
      (fun', args') :: (Expression, [(IsImplicit, Expression)]) <- case fun0 of
        ExpressionHole {} -> (fun0,) <$> mapM (secondM (checkExpression ArityUnknown)) args
        ExpressionIden i -> (fun0,) <$> (idenArity i >>= helper (getLoc i))
        ExpressionLiteral l -> (fun0,) <$> helper (getLoc l) arityLiteral
        ExpressionUniverse l -> (fun0,) <$> helper (getLoc l) arityUniverse
        ExpressionLambda l -> do
          l' <- checkLambda ArityUnknown l
          (ExpressionLambda l',) <$> helper (getLoc l') (arityLambda l')
        ExpressionSimpleLambda {} -> simplelambda
        ExpressionCase l -> do
          l' <- checkCase ArityUnknown l
          (ExpressionCase l',) <$> (arityCase l' >>= helper (getLoc l'))
        ExpressionLet l -> do
          l' <- checkLet ArityUnknown l
          (ExpressionLet l',) <$> (arityLet l' >>= helper (getLoc l'))
        ExpressionFunction f ->
          throw
            ( ErrFunctionApplied
                FunctionApplied
                  { _functionAppliedFunction = f,
                    _functionAppliedArguments = args
                  }
            )
        ExpressionApplication {} -> impossible
      return (foldApplication fun' args')
      where
        helper :: Interval -> Arity -> Sem r [(IsImplicit, Expression)]
        helper i ari = do
          let argsAris :: [Arity]
              argsAris = map toArity (unfoldArity ari)
              toArity :: ArityParameter -> Arity
              toArity = \case
                ParamExplicit a -> a
                ParamImplicit -> ArityUnit
          argsWithHoles :: [(IsImplicit, Expression)] <- addHoles i hintArity ari args
          let argsWithAris :: [(IsImplicit, (Arity, Expression))]
              argsWithAris = [(i', (a, e')) | (a, (i', e')) <- zip (argsAris ++ repeat ArityUnknown) argsWithHoles]
          mapM (secondM (uncurry checkExpression)) argsWithAris
        addHoles ::
          Interval ->
          Arity ->
          Arity ->
          [(IsImplicit, Expression)] ->
          Sem r [(IsImplicit, Expression)]
        addHoles loc hint = go 0
          where
            go ::
              Int ->
              Arity ->
              [(IsImplicit, Expression)] ->
              Sem r [(IsImplicit, Expression)]
            go idx ari goargs = case (ari, goargs) of
              (ArityFunction (FunctionArity ParamImplicit r), (Implicit, e) : rest) ->
                ((Implicit, e) :) <$> go (succ idx) r rest
              (ArityFunction (FunctionArity (ParamExplicit {}) r), (Explicit, e) : rest) ->
                ((Explicit, e) :) <$> go (succ idx) r rest
              (ArityFunction (FunctionArity ParamImplicit _), [])
                -- When there are no remaining arguments and the expected arity of the
                -- expression matches the current arity we should *not* insert a hole.
                | ari == hint -> return []
              (ArityFunction (FunctionArity ParamImplicit r), _) -> do
                h <- newHole loc
                ((Implicit, ExpressionHole h) :) <$> go (succ idx) r goargs
              (ArityFunction (FunctionArity (ParamExplicit {}) _), (Implicit, _) : _) ->
                throw
                  ( ErrExpectedExplicitArgument
                      ExpectedExplicitArgument
                        { _expectedExplicitArgumentApp = (fun0, args),
                          _expectedExplicitArgumentIx = idx
                        }
                  )
              (ArityUnit, []) -> return []
              (ArityFunction (FunctionArity (ParamExplicit _) _), []) -> return []
              (ArityUnit, _ : _) ->
                throw
                  ( ErrTooManyArguments
                      TooManyArguments
                        { _tooManyArgumentsApp = (fun0, args),
                          _tooManyArgumentsUnexpected = length goargs
                        }
                  )
              (ArityUnknown, []) -> return []
              (ArityUnknown, p : ps) -> (p :) <$> go (succ idx) ArityUnknown ps

newHole :: (Member NameIdGen r) => Interval -> Sem r Hole
newHole loc = (`Hole` loc) <$> freshNameId
