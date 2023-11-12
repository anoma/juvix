module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Extra qualified as Internal
import Juvix.Compiler.Internal.Translation.FromConcrete.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.LocalVars
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

type MCache = Cache ModuleIndex Module

checkModule ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, MCache] r) =>
  Module ->
  Sem r Module
checkModule = cacheGet . ModuleIndex

checkModuleIndexNoCache ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, MCache] r) =>
  ModuleIndex ->
  Sem r Module
checkModuleIndexNoCache (ModuleIndex Module {..}) = do
  _moduleBody' <- runReader @InsertedArgsStack mempty (checkModuleBody _moduleBody)
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError, MCache] r) =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkMutualBlock _moduleStatements
  _moduleImports' <- mapM checkImport _moduleImports
  return
    ModuleBody
      { _moduleStatements = _moduleStatements',
        _moduleImports = _moduleImports'
      }

checkModuleIndex ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, MCache] r) =>
  ModuleIndex ->
  Sem r ModuleIndex
checkModuleIndex (ModuleIndex m) = ModuleIndex <$> checkModule m

checkImport ::
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, MCache] r) =>
  Import ->
  Sem r Import
checkImport = traverseOf importModule checkModuleIndex

checkInductive :: forall r. (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => InductiveDef -> Sem r InductiveDef
checkInductive d = do
  let _inductiveName = d ^. inductiveName
      _inductiveBuiltin = d ^. inductiveBuiltin
      _inductivePositive = d ^. inductivePositive
      _inductiveTrait = d ^. inductiveTrait
      _inductivePragmas = d ^. inductivePragmas
  (localVars, _inductiveParameters) <- checkParameters (d ^. inductiveParameters)
  _inductiveExamples <- runReader localVars (mapM checkExample (d ^. inductiveExamples))
  _inductiveConstructors <- runReader localVars (mapM checkConstructor (d ^. inductiveConstructors))
  _inductiveType <- runReader localVars (checkType (d ^. inductiveType))
  return InductiveDef {..}
  where
    checkParameters :: [InductiveParameter] -> Sem r (LocalVars, [InductiveParameter])
    checkParameters = runState emptyLocalVars . mapM checkParam
      where
        checkParam :: InductiveParameter -> Sem (State LocalVars ': r) InductiveParameter
        checkParam = return

checkConstructor :: (Members '[Reader InsertedArgsStack, Reader LocalVars, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => ConstructorDef -> Sem r ConstructorDef
checkConstructor c = do
  let _inductiveConstructorName = c ^. inductiveConstructorName
      _inductiveConstructorPragmas = c ^. inductiveConstructorPragmas
  _inductiveConstructorType <- checkType (c ^. inductiveConstructorType)
  _inductiveConstructorExamples <- mapM checkExample (c ^. inductiveConstructorExamples)
  return ConstructorDef {..}

-- | check the arity of some ty : Type
checkType :: (Members '[Reader InsertedArgsStack, Reader LocalVars, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => Expression -> Sem r Expression
checkType = checkExpression ArityUnit

checkAxiom :: (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => AxiomDef -> Sem r AxiomDef
checkAxiom a = do
  let _axiomName = a ^. axiomName
      _axiomBuiltin = a ^. axiomBuiltin
      _axiomPragmas = a ^. axiomPragmas
  _axiomType <- withEmptyLocalVars (checkType (a ^. axiomType))
  return AxiomDef {..}

checkMutualStatement ::
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  MutualStatement ->
  Sem r MutualStatement
checkMutualStatement = \case
  StatementFunction f -> StatementFunction <$> checkFunctionDef f
  StatementInductive f -> StatementInductive <$> checkInductive f
  StatementAxiom a -> StatementAxiom <$> checkAxiom a

checkMutualBlockLet ::
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  MutualBlockLet ->
  Sem r MutualBlockLet
checkMutualBlockLet (MutualBlockLet funs) = MutualBlockLet <$> mapM checkFunctionDef funs

checkMutualBlock ::
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  MutualBlock ->
  Sem r MutualBlock
checkMutualBlock (MutualBlock funs) = MutualBlock <$> mapM checkMutualStatement funs

checkFunctionDef ::
  forall r.
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  let arity = typeArity _funDefType
  _funDefType' <- withEmptyLocalVars (checkType _funDefType)
  _funDefBody' <- checkFunctionBody arity _funDefBody
  _funDefExamples' <- withEmptyLocalVars (mapM checkExample _funDefExamples)
  let argTys = fst (unfoldFunType _funDefType')
  _funDefArgsInfo' <- withEmptyLocalVars (checkArgsInfo _funDefArgsInfo argTys)
  return
    FunctionDef
      { _funDefBody = _funDefBody',
        _funDefExamples = _funDefExamples',
        _funDefType = _funDefType',
        _funDefArgsInfo = _funDefArgsInfo',
        _funDefName,
        _funDefTerminating,
        _funDefInstance,
        _funDefCoercion,
        _funDefBuiltin,
        _funDefPragmas
      }

checkArgsInfo ::
  forall r.
  (Members '[Reader InsertedArgsStack, NameIdGen, Reader LocalVars, Error ArityCheckerError, Reader InfoTable] r) =>
  [ArgInfo] ->
  [FunctionParameter] ->
  Sem r [ArgInfo]
checkArgsInfo defaults =
  execOutputList
    . go defaults
  where
    go :: [ArgInfo] -> [FunctionParameter] -> Sem (Output ArgInfo ': r) ()
    go = \case
      [] -> const (return ())
      d : ds' -> \case
        [] -> impossible
        p : ps' -> do
          let ari = typeArity (p ^. paramType)
          dval <- case (d ^. argInfoDefault, p ^. paramImplicit) of
            (Nothing, _) -> return Nothing
            (Just val, Implicit) ->
              Just <$> checkExpression ari val
            (Just {}, _) -> impossible
          output (set argInfoDefault dval d)
          withLocalVarMaybe ari (p ^. paramName) (go ds' ps')

checkFunctionBody ::
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError] r) =>
  Arity ->
  Expression ->
  Sem r Expression
checkFunctionBody ari body = do
  case body of
    ExpressionLambda {} ->
      withEmptyLocalVars (checkExpression ari body)
    _ -> do
      hint <- guessArity body
      (patterns', locals, bodyAri) <- checkLhs (getLoc body) hint ari []
      body' <- runReader locals (checkExpression bodyAri body)
      return $ case nonEmpty patterns' of
        Nothing -> body'
        Just lambdaPatterns' ->
          ExpressionLambda
            Lambda
              { _lambdaType = Nothing,
                _lambdaClauses =
                  pure
                    LambdaClause
                      { _lambdaPatterns = lambdaPatterns',
                        _lambdaBody = body'
                      }
              }

simplelambda :: a
simplelambda = error "simple lambda expressions are not supported by the arity checker"

withLocalVarMaybe :: (Members '[Reader LocalVars] r) => Arity -> Maybe VarName -> Sem r a -> Sem r a
withLocalVarMaybe ari mv = case mv of
  Nothing -> id
  Just v -> withLocalVar ari v

withLocalVar :: (Members '[Reader LocalVars] r) => Arity -> VarName -> Sem r a -> Sem r a
withLocalVar ari v = local (withArity v ari)

withEmptyLocalVars :: Sem (Reader LocalVars ': r) a -> Sem r a
withEmptyLocalVars = runReader emptyLocalVars

arityLet :: (Members '[Reader InfoTable] r) => Let -> Sem r Arity
arityLet l = guessArity (l ^. letExpression)

inferReplExpression :: (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r) => Expression -> Sem r Expression
inferReplExpression e = do
  ari <- guessArity e
  withEmptyLocalVars
    . runReader @InsertedArgsStack mempty
    $ checkExpression ari e

guessArity ::
  forall r.
  (Members '[Reader InfoTable] r) =>
  Expression ->
  Sem r Arity
guessArity = \case
  ExpressionHole {} -> return ArityUnknown
  ExpressionInstanceHole {} -> return ArityUnit
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
        (f, args) = second (map (^. appArgIsImplicit) . toList) (unfoldApplication' a)

        refine :: [IsImplicit] -> [ArityParameter] -> Maybe [ArityParameter]
        refine as ps = case (as, ps) of
          (Explicit : as', ArityParameter {_arityParameterImplicit = Explicit} : ps') -> refine as' ps'
          (Implicit : as', ArityParameter {_arityParameterImplicit = Implicit} : ps') -> refine as' ps'
          (ImplicitInstance : as', ArityParameter {_arityParameterImplicit = ImplicitInstance} : ps') -> refine as' ps'
          (as'@(Explicit : _), ArityParameter {_arityParameterImplicit = Implicit} : ps') -> refine as' ps'
          (as'@(Explicit : _), ArityParameter {_arityParameterImplicit = ImplicitInstance} : ps') -> refine as' ps'
          (Implicit : _, ArityParameter {_arityParameterImplicit = Explicit} : _) -> Nothing
          (ImplicitInstance : _, ArityParameter {_arityParameterImplicit = Explicit} : _) -> Nothing
          (Implicit : _, ArityParameter {_arityParameterImplicit = ImplicitInstance} : _) -> Nothing
          (ImplicitInstance : _, ArityParameter {_arityParameterImplicit = Implicit} : _) -> Nothing
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
arityCase :: (Members '[Reader InfoTable] r) => Case -> Sem r Arity
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
      { _ufoldArityParams =
          replicate
            (maximum1 (fmap numPatterns (l ^. lambdaClauses)))
            ( ArityParameter
                { _arityParameterArity = ArityUnknown,
                  _arityParameterImplicit = Explicit,
                  _arityParameterInfo = emptyArgInfo
                }
            ),
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
          let n = length tailUnderscores
              a' = foldArity (over ufoldArityParams (drop n) (unfoldArity' a))
          wildcards <- mapM genWildcard' tailUnderscores
          return (wildcards, a')
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
          case (p ^. patternArgIsImplicit, l ^. arityParameterImplicit) of
            (Implicit, Implicit {}) -> do
              b' <- checkPattern (arityParameter l) p
              first (b' :) <$> goLhs r ps
            (Implicit, Explicit {}) ->
              throw
                ( ErrWrongPatternIsImplicit
                    WrongPatternIsImplicit
                      { _wrongPatternIsImplicitExpected = Explicit,
                        _wrongPatternIsImplicitActual = p
                      }
                )
            (Implicit, ImplicitInstance {}) ->
              throw
                ( ErrWrongPatternIsImplicit
                    WrongPatternIsImplicit
                      { _wrongPatternIsImplicitExpected = ImplicitInstance,
                        _wrongPatternIsImplicitActual = p
                      }
                )
            (ImplicitInstance, ImplicitInstance) -> do
              b' <- checkPattern (arityParameter l) p
              first (b' :) <$> goLhs r ps
            (ImplicitInstance, Explicit {}) ->
              throw
                ( ErrWrongPatternIsImplicit
                    WrongPatternIsImplicit
                      { _wrongPatternIsImplicitExpected = Explicit,
                        _wrongPatternIsImplicitActual = p
                      }
                )
            (ImplicitInstance, Implicit {}) -> do
              wildcard <- genWildcard' Implicit
              first (wildcard :) <$> goLhs r lhs
            (Explicit, Implicit {}) -> do
              wildcard <- genWildcard' Implicit
              first (wildcard :) <$> goLhs r lhs
            (Explicit, ImplicitInstance) -> do
              wildcard <- genWildcard' ImplicitInstance
              first (wildcard :) <$> goLhs r lhs
            (Explicit, Explicit) -> do
              p' <- checkPattern (l ^. arityParameterArity) p
              first (p' :) <$> goLhs r ps
      where
        genWildcard' :: forall r'. (Members '[NameIdGen] r') => IsImplicit -> Sem r' PatternArg
        genWildcard' = genWildcard loc

    -- This is an heuristic and it can have an undesired result.
    -- Sometimes the outcome may even be confusing.
    tailHelper :: Arity -> Maybe [IsImplicit]
    tailHelper a
      | 0 < pref = Just pref'
      | otherwise = Nothing
      where
        pref' :: [IsImplicit]
        pref' = map paramToImplicit (take pref (unfoldArity a))
        pref :: Int
        pref = aI - targetI
        preceedingImplicits :: Arity -> Int
        preceedingImplicits = length . takeWhile (isImplicitOrInstance . (^. arityParameterImplicit)) . unfoldArity
        aI :: Int
        aI = preceedingImplicits a
        targetI :: Int
        targetI = preceedingImplicits guessedBody
        paramToImplicit :: ArityParameter -> IsImplicit
        paramToImplicit = (^. arityParameterImplicit)

checkPattern ::
  forall r.
  (Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars, NameIdGen] r) =>
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
      PatternWildcardConstructor v -> PatternConstructorApp <$> checkWildcardConstructor v
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

checkWildcardConstructor ::
  forall r.
  (Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, State LocalVars] r) =>
  WildcardConstructor ->
  Sem r ConstructorApp
checkWildcardConstructor w = do
  let c = w ^. wildcardConstructor
  numArgs <- length . constructorArgs . (^. constructorInfoType) <$> lookupConstructor c
  holeArgs <- replicateM numArgs (genWildcard (getLoc w) Explicit)
  return
    ConstructorApp
      { _constrAppConstructor = c,
        _constrAppParameters = holeArgs,
        _constrAppType = Nothing
      }

checkConstructorApp ::
  forall r.
  (Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars, NameIdGen] r) =>
  ConstructorApp ->
  Sem r ConstructorApp
checkConstructorApp ca = do
  let c = ca ^. constrAppConstructor
  args <- constructorArgs . (^. constructorInfoType) <$> lookupConstructor c
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
  (Members '[Reader InsertedArgsStack, Error ArityCheckerError, Reader LocalVars, Reader InfoTable, NameIdGen] r) =>
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
  (Members '[Reader InsertedArgsStack, Error ArityCheckerError, Reader LocalVars, Reader InfoTable, NameIdGen] r) =>
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
      LetMutualBlock f -> LetMutualBlock <$> checkMutualBlockLet f

checkLambda ::
  forall r.
  (Members '[Reader InsertedArgsStack, Error ArityCheckerError, Reader LocalVars, Reader InfoTable, NameIdGen] r) =>
  Arity ->
  Lambda ->
  Sem r Lambda
checkLambda ari l = do
  let _lambdaType = l ^. lambdaType
  _lambdaClauses <- mapM checkLambdaClause (l ^. lambdaClauses)
  return Lambda {..}
  where
    checkLambdaClause ::
      LambdaClause ->
      Sem r LambdaClause
    checkLambdaClause cl = do
      hint <- guessArity (cl ^. lambdaBody)
      (patterns', locals, bodyAri) <- checkLhs loc hint ari (toList (cl ^. lambdaPatterns))
      body' <- runReader locals (checkExpression bodyAri (cl ^. lambdaBody))
      return
        LambdaClause
          { _lambdaPatterns = nonEmpty' patterns',
            _lambdaBody = body'
          }
      where
        loc = getLoc cl

idenArity :: (Members '[Reader LocalVars, Reader InfoTable] r) => Iden -> Sem r Arity
idenArity = \case
  IdenVar v -> getLocalArity v
  IdenInductive i -> typeArity <$> lookupInductiveType i
  IdenFunction f -> do
    fun <- (^. functionInfoDef) <$> lookupFunction f
    let ari = typeArity (fun ^. funDefType)
        defaults = fun ^. funDefArgsInfo
    return (addArgsInfo defaults ari)
  IdenConstructor c -> typeArity <$> lookupConstructorType c
  IdenAxiom a -> typeArity . (^. axiomInfoDef . axiomType) <$> lookupAxiom a

addArgsInfo :: [ArgInfo] -> Arity -> Arity
addArgsInfo = unfoldingArity . helper
  where
    helper :: [ArgInfo] -> UnfoldedArity -> UnfoldedArity
    helper = over ufoldArityParams . go

    go :: [ArgInfo] -> [ArityParameter] -> [ArityParameter]
    go infos params = case infos of
      [] -> params
      info : infos' -> case params of
        [] -> impossible
        para : params' ->
          set arityParameterInfo info para : go infos' params'

-- | let x be some expression of type T. The argument of this function is T and it returns
-- the arity of x. In other words, given (T : Type), it returns the arity of the elements of T.
typeArity :: Expression -> Arity
typeArity = go
  where
    go :: Expression -> Arity
    go = \case
      ExpressionIden i -> goIden i
      ExpressionApplication a -> goApplication a
      ExpressionLiteral {} -> ArityUnknown
      ExpressionFunction f -> ArityFunction (goFun f)
      ExpressionHole {} -> ArityUnknown
      ExpressionInstanceHole {} -> ArityUnit
      ExpressionLambda {} -> ArityUnknown
      ExpressionCase {} -> ArityUnknown
      ExpressionUniverse {} -> ArityUnit
      ExpressionSimpleLambda {} -> simplelambda
      ExpressionLet l -> goLet l

    goApplication :: Application -> Arity
    goApplication a = case lhs of
      ExpressionIden IdenInductive {} -> ArityUnit
      _ -> ArityUnknown
      where
        lhs :: Expression
        lhs = fst (unfoldApplication a)

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
    goParam FunctionParameter {..} =
      ArityParameter
        { _arityParameterArity = case _paramImplicit of
            Explicit -> go _paramType
            Implicit -> go _paramType
            ImplicitInstance -> ArityUnit,
          _arityParameterImplicit = _paramImplicit,
          _arityParameterInfo = emptyArgInfo
        }

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
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError, Reader LocalVars] r) =>
  Example ->
  Sem r Example
checkExample = traverseOf exampleExpression (checkExpression ArityUnknown)

checkExpression ::
  forall r.
  (Members '[Reader InsertedArgsStack, Reader InfoTable, NameIdGen, Error ArityCheckerError, Reader LocalVars] r) =>
  Arity ->
  Expression ->
  Sem r Expression
checkExpression hintArity expr = case expr of
  ExpressionIden {} -> goApp expr []
  ExpressionApplication a -> uncurry goApp $ second toList (unfoldApplication' a)
  ExpressionLiteral {} -> appHelper expr []
  ExpressionFunction f -> ExpressionFunction <$> goFunction f
  ExpressionUniverse {} -> return expr
  ExpressionHole {} -> return expr
  ExpressionInstanceHole {} -> return expr
  ExpressionSimpleLambda {} -> simplelambda
  ExpressionLambda l -> ExpressionLambda <$> checkLambda hintArity l
  ExpressionLet l -> ExpressionLet <$> checkLet hintArity l
  ExpressionCase l -> ExpressionCase <$> checkCase hintArity l
  where
    goFunction :: Function -> Sem r Function
    goFunction (Function l r) = do
      l' <- goFunctionParameter l
      let ari = typeArity (l' ^. paramType)
      r' <- maybe id (withLocalVar ari) (l ^. paramName) (checkType r)
      return (Function l' r')
      where
        goFunctionParameter :: FunctionParameter -> Sem r FunctionParameter
        goFunctionParameter p = do
          let _paramName = p ^. paramName
              _paramImplicit = p ^. paramImplicit
          _paramType <- checkType (p ^. paramType)
          return FunctionParameter {..}

    goApp :: Expression -> [ApplicationArg] -> Sem r Expression
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

    goBuiltinApp :: Name -> Int -> Int -> Expression -> [ApplicationArg] -> Sem r Expression
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
        goImplArgs :: Int -> [ApplicationArg] -> Sem r [ApplicationArg]
        goImplArgs 0 as = return as
        goImplArgs k ((ApplicationArg Implicit _) : as) = goImplArgs (k - 1) as
        goImplArgs _ as = return as

    appHelper :: Expression -> [ApplicationArg] -> Sem r Expression
    appHelper fun0 args = do
      (fun', args') :: (Expression, [ApplicationArg]) <- case fun0 of
        ExpressionHole {} -> (fun0,) <$> mapM (traverseOf appArg (checkExpression ArityUnknown)) args
        ExpressionInstanceHole {} -> (fun0,) <$> mapM (traverseOf appArg (checkExpression ArityUnknown)) args
        ExpressionIden i -> (,[]) <$> goAppLeftIden i
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
        goAppLeftIden :: Iden -> Sem r Expression
        goAppLeftIden i = case i of
          IdenFunction f -> do
            infos <- (^. functionInfoDef . funDefArgsInfo) <$> lookupFunction f
            let hasADefault = has (each . argInfoDefault . _Just) infos
            if
                | hasADefault -> goAppLeftIdenWithDefaults i
                | otherwise -> noDefaults
          _ -> noDefaults
          where
            noDefaults :: Sem r Expression
            noDefaults = do
              args' :: [ApplicationArg] <- map (^. insertedArg) <$> (idenArity i >>= helperDefaultArgs (getLoc i))
              return (foldApplication fun0 args')

        goAppLeftIdenWithDefaults :: Iden -> Sem r Expression
        goAppLeftIdenWithDefaults i = do
          namedArgs :: [InsertedArg] <- idenArity i >>= helperDefaultArgs (getLoc i)
          case nonEmpty namedArgs of
            Nothing -> return (toExpression i)
            Just args' -> do
              let mkClause :: InsertedArg -> Sem r Internal.PreLetStatement
                  mkClause InsertedArg {..} = do
                    -- TODO put actual type instead of hole?
                    let arg = _insertedArg
                        nm = _insertedArgName
                    ty <- mkFreshHole (getLoc arg)
                    return (Internal.PreLetFunctionDef (Internal.simpleFunDef nm ty (arg ^. appArg)))
                  mkAppArg :: InsertedArg -> ApplicationArg
                  mkAppArg InsertedArg {..} =
                    ApplicationArg
                      { _appArgIsImplicit = _insertedArg ^. appArgIsImplicit,
                        _appArg = toExpression _insertedArgName
                      }
              clauses :: NonEmpty Internal.LetClause <- nonEmpty' . Internal.mkLetClauses <$> mapM mkClause args'
              let app = foldApplication (toExpression fun0) (map mkAppArg namedArgs)
              letexpr <-
                Internal.substitutionE (renameKind KNameFunction (map (^. insertedArgName) namedArgs)) $
                  ExpressionLet
                    Let
                      { _letClauses = clauses,
                        _letExpression = app
                      }
              Internal.clone letexpr

        helper :: Interval -> Arity -> Sem r [ApplicationArg]
        helper i ari = map (^. insertedArg) <$> helperDefaultArgs i ari

        helperDefaultArgs :: Interval -> Arity -> Sem r [InsertedArg]
        helperDefaultArgs i ari = do
          let argsAris :: [Arity]
              argsAris = map arityParameter (unfoldArity ari)
          argsWithHoles :: [InsertedArg] <- addHoles i hintArity ari args
          let argsWithAris :: [(InsertedArg, Arity)]
              argsWithAris = zip argsWithHoles (argsAris ++ repeat ArityUnknown)
          forM argsWithAris $ \(ia, argAri) -> do
            checkDefaultArgCycle ia
            let adjustCtx
                  | ia ^. insertedArgDefault = over insertedArgsStack ((ia ^. insertedArgName) :)
                  | otherwise = id
            local adjustCtx (traverseOf (insertedArg . appArg) (checkExpression argAri) ia)
          where
            checkDefaultArgCycle :: InsertedArg -> Sem r ()
            checkDefaultArgCycle ia = do
              st <- asks (^. insertedArgsStack)
              case span (/= (ia ^. insertedArgName)) st of
                (_, []) -> return ()
                (c, _) ->
                  let cyc = NonEmpty.reverse (ia ^. insertedArgName :| c)
                   in throw (ErrDefaultArgCycle (DefaultArgCycle cyc))

        addHoles ::
          Interval ->
          Arity ->
          Arity ->
          [ApplicationArg] ->
          Sem r [InsertedArg]
        addHoles loc hint ari0 = evalState 0 . execOutputList . go ari0
          where
            go ::
              Arity ->
              [ApplicationArg] ->
              Sem (Output InsertedArg ': State Int ': r) ()
            go ari goargs = do
              let emitNoName :: Bool -> ApplicationArg -> Sem (Output InsertedArg ': State Int ': r) ()
                  emitNoName isDef x = do
                    let l = getLoc x
                    v <- freshFunVar l "gen_helper"
                    emit isDef v x
                  emitWithParameter :: Bool -> ArityParameter -> ApplicationArg -> Sem (Output InsertedArg ': State Int ': r) ()
                  emitWithParameter isDef p = maybe (emitNoName isDef) (emit isDef) (p ^. arityParameterName)
                  emitInstanceHole :: Sem (Output InsertedArg ': State Int ': r) ()
                  emitInstanceHole = do
                    h <- newHoleInstance loc
                    emitNoName False (ApplicationArg ImplicitInstance (ExpressionInstanceHole h))
                  emitImplicitHole :: ArityParameter -> Sem (Output InsertedArg ': State Int ': r) ()
                  emitImplicitHole p = do
                    (isDef, h) <- newHoleImplicit p loc
                    emitWithParameter isDef p (ApplicationArg Implicit h)
                  emit :: Bool -> Name -> ApplicationArg -> Sem (Output InsertedArg ': State Int ': r) ()
                  emit isDef n x = do
                    output
                      InsertedArg
                        { _insertedArg = x,
                          _insertedArgDefault = isDef,
                          _insertedArgName = n
                        }
                    modify' @Int succ
              case (ari, goargs) of
                (ArityFunction (FunctionArity (p@ArityParameter {_arityParameterImplicit = Implicit}) r), (ApplicationArg Implicit e) : rest) -> do
                  emitWithParameter False p (ApplicationArg Implicit e)
                  go r rest
                (ArityFunction (FunctionArity (ArityParameter {_arityParameterImplicit = ImplicitInstance}) r), (ApplicationArg ImplicitInstance e) : rest) -> do
                  emitNoName False (ApplicationArg ImplicitInstance e)
                  go r rest
                (ArityFunction (FunctionArity (p@ArityParameter {_arityParameterImplicit = Explicit}) r), (ApplicationArg Explicit e) : rest) -> do
                  emitWithParameter False p (ApplicationArg Explicit e)
                  go r rest
                (ArityFunction (FunctionArity impl _), [])
                  -- When there are no remaining arguments and the expected arity of the
                  -- expression matches the current arity we should *not* insert a hole.
                  | arityParameterImplicitOrInstance impl
                      && ari == hint ->
                      return ()
                (ArityFunction (FunctionArity (p@ArityParameter {_arityParameterImplicit = Implicit}) r), _) -> do
                  -- h <- newHoleImplicit p loc
                  -- emitWithParameter p (ApplicationArg Implicit h)
                  emitImplicitHole p
                  go r goargs
                (ArityFunction (FunctionArity (ArityParameter {_arityParameterImplicit = ImplicitInstance}) r), _) -> do
                  emitInstanceHole
                  go r goargs
                (ArityFunction (FunctionArity (ArityParameter {_arityParameterImplicit = Explicit}) _), (ApplicationArg _ _) : _) -> do
                  idx <- get @Int
                  throw
                    ( ErrExpectedExplicitArgument
                        ExpectedExplicitArgument
                          { _expectedExplicitArgumentApp = (fun0, args),
                            _expectedExplicitArgumentIx = idx
                          }
                    )
                (ArityUnit, []) -> return ()
                (ArityFunction (FunctionArity (ArityParameter {_arityParameterImplicit = Explicit}) _), []) -> return ()
                (ArityUnit, _ : _) ->
                  throw
                    ( ErrTooManyArguments
                        TooManyArguments
                          { _tooManyArgumentsApp = (fun0, args),
                            _tooManyArgumentsUnexpected = length goargs
                          }
                    )
                (ArityUnknown, []) -> return ()
                (ArityUnknown, p : ps) -> do
                  emitNoName False p
                  go ArityUnknown ps

newHoleImplicit :: (Member NameIdGen r) => ArityParameter -> Interval -> Sem r (Bool, Expression)
newHoleImplicit i loc = case i ^. arityParameterInfo . argInfoDefault of
  Nothing -> (False,) . ExpressionHole . mkHole loc <$> freshNameId
  Just e -> do
    -- TODO update location
    return (True, e)

newHoleInstance :: (Member NameIdGen r) => Interval -> Sem r InstanceHole
newHoleInstance loc = mkInstanceHole loc <$> freshNameId
