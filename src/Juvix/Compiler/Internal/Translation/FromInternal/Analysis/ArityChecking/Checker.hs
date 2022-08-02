module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Checker
  ( module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Checker,
    module Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error,
  )
where

import Juvix.Compiler.Internal.Extra
import Juvix.Compiler.Internal.Translation.FromAbstract.Data.Context
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.LocalVars
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Data.Types
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.ArityChecking.Error
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude hiding (fromEither)

checkModule ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
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
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  arity <- typeArity _funDefType
  _funDefClauses' <- mapM (checkFunctionClause arity) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkFunctionClause ::
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
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

lambda :: a
lambda = error "lambda expressions are not supported by the arity checker"

guessArity ::
  forall r.
  Members '[Reader InfoTable] r =>
  Expression ->
  Sem r (Maybe Arity)
guessArity = \case
  ExpressionHole {} -> return Nothing
  ExpressionFunction {} -> return (Just ArityUnit)
  ExpressionLiteral {} -> return (Just arityLiteral)
  ExpressionApplication a -> appHelper a
  ExpressionIden i -> idenHelper i
  ExpressionUniverse {} -> return (Just arityUniverse)
  ExpressionLambda {} -> lambda
  where
    idenHelper :: Iden -> Sem r (Maybe Arity)
    idenHelper i = case i of
      IdenVar {} -> return Nothing
      _ -> Just <$> runReader (LocalVars mempty) (idenArity i)

    appHelper :: Application -> Sem r (Maybe Arity)
    appHelper a = do
      f' <- arif
      return (f' >>= \f'' -> foldArity <$> refine (unfoldArity f'') args)
      where
        (f, args) = second (map fst . toList) (unfoldApplication' a)

        refine :: [ArityParameter] -> [IsImplicit] -> Maybe [ArityParameter]
        refine ps as = case (ps, as) of
          (ParamExplicit {} : ps', Explicit : as') -> refine ps' as'
          (ParamImplicit {} : ps', Implicit : as') -> refine ps' as'
          (ParamImplicit {} : ps', as'@(Explicit : _)) -> refine ps' as'
          (ParamExplicit {} : _, Implicit : _) -> Nothing
          (ps', []) -> Just ps'
          ([], _ : _) -> Nothing

        arif :: Sem r (Maybe Arity)
        arif = case f of
          ExpressionHole {} -> return Nothing
          ExpressionUniverse {} -> return (Just arityUniverse)
          ExpressionApplication {} -> impossible
          ExpressionFunction {} -> return (Just ArityUnit)
          ExpressionLiteral {} -> return (Just arityLiteral)
          ExpressionIden i -> idenHelper i
          ExpressionLambda {} -> lambda

-- | The arity of all literals is assumed to be: {} -> 1
arityLiteral :: Arity
arityLiteral =
  ArityFunction
    FunctionArity
      { _functionArityLeft = ParamImplicit,
        _functionArityRight = ArityUnit
      }

arityUniverse :: Arity
arityUniverse = ArityUnit

checkLhs ::
  forall r.
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError] r =>
  Interval ->
  Maybe Arity ->
  Arity ->
  [PatternArg] ->
  Sem r ([PatternArg], LocalVars, Arity)
checkLhs loc hint ariSignature pats = do
  (locals, (pats', bodyAri)) <- runState emptyLocalVars (goLhs ariSignature pats)
  return (pats', locals, bodyAri)
  where
    -- returns the expanded patterns and the rest of the Arity (the arity of the
    -- body once all the patterns have been processed).
    -- Does not insert holes greedily. I.e. implicit wildcards are only inserted
    -- between explicit parameters already in the pattern.
    goLhs :: Arity -> [PatternArg] -> Sem (State LocalVars ': r) ([PatternArg], Arity)
    goLhs a = \case
      [] -> return $ case hint >>= tailHelper a of
        Nothing -> ([], a)
        Just tailUnderscores ->
          let a' = foldArity (drop tailUnderscores (unfoldArity a))
           in (replicate tailUnderscores wildcard, a')
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
            (Explicit, ParamImplicit) ->
              first (wildcard :) <$> goLhs r lhs
            (Explicit, ParamExplicit pa) -> do
              p' <- checkPattern pa p
              first (p' :) <$> goLhs r ps
      where
        wildcard :: PatternArg
        wildcard = PatternArg Implicit (PatternWildcard (Wildcard loc))

    tailHelper :: Arity -> Arity -> Maybe Int
    tailHelper a target
      | notNull a' && all (== ParamImplicit) a' = Just (length a')
      | otherwise = Nothing
      where
        a' = dropSuffix target' (unfoldArity a)
        target' = unfoldArity target

checkPattern ::
  forall r.
  Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars] r =>
  Arity ->
  PatternArg ->
  Sem r PatternArg
checkPattern ari = traverseOf patternArgPattern helper
  where
    helper :: Pattern -> Sem r Pattern
    helper = \case
      PatternVariable v -> addArity v ari $> PatternVariable v
      PatternWildcard i -> return (PatternWildcard i)
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
  Members '[Reader InfoTable, Error ArityCheckerError, State LocalVars] r =>
  ConstructorApp ->
  Sem r ConstructorApp
checkConstructorApp ca@(ConstructorApp c ps) = do
  args <- (^. constructorInfoArgs) <$> lookupConstructor c
  arities <- mapM typeArity args
  let n = length arities
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
  return (ConstructorApp c ps')

idenArity :: Members '[Reader LocalVars, Reader InfoTable] r => Iden -> Sem r Arity
idenArity = \case
  IdenFunction f -> lookupFunction f >>= typeArity . (^. functionInfoDef . funDefType)
  IdenConstructor c -> constructorType c >>= typeArity
  IdenVar v -> getLocalArity v
  IdenAxiom a -> lookupAxiom a >>= typeArity . (^. axiomInfoType)
  IdenInductive i -> inductiveType i >>= typeArity

-- | let x be some expression of type T. The argument of this function is T and it returns
-- the arity of x.
typeArity :: forall r. Members '[Reader InfoTable] r => Expression -> Sem r Arity
typeArity = go
  where
    go :: Expression -> Sem r Arity
    go = \case
      ExpressionIden i -> goIden i
      ExpressionApplication {} -> return ArityUnit
      ExpressionLiteral {} -> return ArityUnknown
      ExpressionFunction f -> ArityFunction <$> goFun2 f
      ExpressionHole {} -> return ArityUnknown
      ExpressionUniverse {} -> return ArityUnit
      ExpressionLambda {} -> lambda

    goIden :: Iden -> Sem r Arity
    goIden = \case
      IdenVar {} -> return ArityUnknown
      IdenInductive {} -> return ArityUnit
      IdenFunction {} -> return ArityUnknown -- we need normalization to determine the arity
      IdenConstructor {} -> return ArityUnknown -- will be a type error
      IdenAxiom ax -> lookupAxiom ax >>= go . (^. axiomInfoType)

    goParam :: FunctionParameter -> Sem r ArityParameter
    goParam (FunctionParameter _ i e) =
      case i of
        Implicit -> return ParamImplicit
        Explicit -> ParamExplicit <$> go e

    goFun2 :: Function -> Sem r FunctionArity
    goFun2 (Function l r) = do
      l' <- goParam l
      r' <- go r
      return
        FunctionArity
          { _functionArityLeft = l',
            _functionArityRight = r'
          }

checkExpression ::
  forall r.
  Members '[Reader InfoTable, NameIdGen, Error ArityCheckerError, Reader LocalVars] r =>
  Arity ->
  Expression ->
  Sem r Expression
checkExpression hintArity expr = case expr of
  ExpressionIden {} -> appHelper expr []
  ExpressionApplication a -> goApp a
  ExpressionLiteral {} -> appHelper expr []
  ExpressionFunction {} -> return expr
  ExpressionUniverse {} -> return expr
  ExpressionHole {} -> return expr
  ExpressionLambda {} -> lambda
  where
    goApp :: Application -> Sem r Expression
    goApp = uncurry appHelper . second toList . unfoldApplication'

    appHelper :: Expression -> [(IsImplicit, Expression)] -> Sem r Expression
    appHelper fun args = do
      args' :: [(IsImplicit, Expression)] <- case fun of
        ExpressionHole {} -> mapM (secondM (checkExpression ArityUnknown)) args
        ExpressionIden i -> idenArity i >>= helper (getLoc i)
        ExpressionLiteral l -> helper (getLoc l) arityLiteral
        ExpressionUniverse l -> helper (getLoc l) arityUniverse
        ExpressionLambda {} -> lambda
        ExpressionFunction f ->
          throw
            ( ErrFunctionApplied
                FunctionApplied
                  { _functionAppliedFunction = f,
                    _functionAppliedArguments = args
                  }
            )
        ExpressionApplication {} -> impossible
      return (foldApplication fun args')
      where
        helper :: Interval -> Arity -> Sem r [(IsImplicit, Expression)]
        helper i ari = do
          let argsAris :: [Arity]
              argsAris = map toArity (unfoldArity ari)
              toArity :: ArityParameter -> Arity
              toArity = \case
                ParamExplicit a -> a
                ParamImplicit -> ArityUnit
          mapM
            (secondM (uncurry checkExpression))
            [(i', (a, e')) | (a, (i', e')) <- zip (argsAris ++ repeat ArityUnknown) args]
            >>= addHoles i hintArity ari
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
                        { _expectedExplicitArgumentApp = (fun, args),
                          _expectedExplicitArgumentIx = idx
                        }
                  )
              (ArityUnit, []) -> return []
              (ArityFunction (FunctionArity (ParamExplicit _) _), []) -> return []
              (ArityUnit, _ : _) ->
                throw
                  ( ErrTooManyArguments
                      TooManyArguments
                        { _tooManyArgumentsApp = (fun, args),
                          _tooManyArgumentsUnexpected = length goargs
                        }
                  )
              (ArityUnknown, []) -> return []
              (ArityUnknown, p : ps) -> (p :) <$> go (succ idx) ArityUnknown ps

newHole :: Member NameIdGen r => Interval -> Sem r Hole
newHole loc = (`Hole` loc) <$> freshNameId
