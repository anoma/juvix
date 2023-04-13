module Juvix.Compiler.Internal.Extra
  ( module Juvix.Compiler.Internal.Extra,
    module Juvix.Compiler.Internal.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

type SubsE = HashMap VarName Expression

type Rename = HashMap VarName VarName

type Subs = HashMap VarName Expression

type VarMap = HashMap VarName VarName

class HasExpressions a where
  leafExpressions :: Traversal' a Expression

instance HasExpressions LambdaClause where
  leafExpressions f l = do
    _lambdaPatterns <- traverse (leafExpressions f) (l ^. lambdaPatterns)
    _lambdaBody <- leafExpressions f (l ^. lambdaBody)
    pure LambdaClause {..}

instance HasExpressions Lambda where
  leafExpressions f l = do
    _lambdaClauses <- traverse (leafExpressions f) (l ^. lambdaClauses)
    _lambdaType <- traverse (leafExpressions f) (l ^. lambdaType)
    pure Lambda {..}

instance HasExpressions Expression where
  leafExpressions f e = case e of
    ExpressionIden {} -> f e
    ExpressionApplication a -> ExpressionApplication <$> leafExpressions f a
    ExpressionFunction fun -> ExpressionFunction <$> leafExpressions f fun
    ExpressionSimpleLambda l -> ExpressionSimpleLambda <$> leafExpressions f l
    ExpressionLambda l -> ExpressionLambda <$> leafExpressions f l
    ExpressionLet l -> ExpressionLet <$> leafExpressions f l
    ExpressionCase c -> ExpressionCase <$> leafExpressions f c
    ExpressionLiteral {} -> f e
    ExpressionUniverse {} -> f e
    ExpressionHole {} -> f e

instance HasExpressions ConstructorApp where
  leafExpressions f = traverseOf (constrAppType . _Just) (leafExpressions f)

instance HasExpressions PatternArg where
  leafExpressions f = traverseOf patternArgPattern (leafExpressions f)

instance HasExpressions Pattern where
  leafExpressions f p = case p of
    PatternVariable {} -> pure p
    PatternConstructorApp a -> PatternConstructorApp <$> leafExpressions f a

instance HasExpressions CaseBranch where
  leafExpressions f b = do
    _caseBranchPattern <- leafExpressions f (b ^. caseBranchPattern)
    _caseBranchExpression <- leafExpressions f (b ^. caseBranchExpression)
    pure CaseBranch {..}

instance HasExpressions Case where
  leafExpressions f l = do
    _caseBranches :: NonEmpty CaseBranch <- traverse (leafExpressions f) (l ^. caseBranches)
    _caseExpression <- leafExpressions f (l ^. caseExpression)
    _caseExpressionType <- traverse (leafExpressions f) (l ^. caseExpressionType)
    _caseExpressionWholeType <- traverse (leafExpressions f) (l ^. caseExpressionWholeType)
    pure Case {..}
    where
      _caseParens = l ^. caseParens

instance HasExpressions MutualBlock where
  leafExpressions f (MutualBlock defs) =
    MutualBlock <$> traverse (leafExpressions f) defs

instance HasExpressions LetClause where
  leafExpressions f = \case
    LetFunDef d -> LetFunDef <$> leafExpressions f d
    LetMutualBlock b -> LetMutualBlock <$> leafExpressions f b

instance HasExpressions Let where
  leafExpressions f l = do
    _letClauses :: NonEmpty LetClause <- traverse (leafExpressions f) (l ^. letClauses)
    _letExpression <- leafExpressions f (l ^. letExpression)
    pure Let {..}

instance HasExpressions TypedExpression where
  leafExpressions f t@TypedExpression {..} = do
    e' <- leafExpressions f _typedExpression
    pure (t {_typedExpression = e'})

instance HasExpressions SimpleLambda where
  leafExpressions f (SimpleLambda v ty b) = do
    b' <- leafExpressions f b
    ty' <- leafExpressions f ty
    pure (SimpleLambda v ty' b')

instance HasExpressions FunctionParameter where
  leafExpressions f (FunctionParameter m i e) = do
    e' <- leafExpressions f e
    pure (FunctionParameter m i e')

instance HasExpressions Function where
  leafExpressions f (Function l r) = do
    l' <- leafExpressions f l
    r' <- leafExpressions f r
    pure (Function l' r')

instance HasExpressions Application where
  leafExpressions f (Application l r i) = do
    l' <- leafExpressions f l
    r' <- leafExpressions f r
    pure (Application l' r' i)

-- | Prism
_ExpressionHole :: Traversal' Expression Hole
_ExpressionHole f e = case e of
  ExpressionHole h -> ExpressionHole <$> f h
  _ -> pure e

holes :: (HasExpressions a) => Traversal' a Hole
holes = leafExpressions . _ExpressionHole

hasHoles :: (HasExpressions a) => a -> Bool
hasHoles = has holes

subsHoles :: (HasExpressions a) => HashMap Hole Expression -> a -> a
subsHoles s = over leafExpressions helper
  where
    helper :: Expression -> Expression
    helper e = case e of
      ExpressionHole h -> fromMaybe e (s ^. at h)
      _ -> e

instance HasExpressions FunctionClause where
  leafExpressions f c = do
    _clauseBody <- leafExpressions f (c ^. clauseBody)
    _clausePatterns <- traverse (leafExpressions f) (c ^. clausePatterns)
    pure FunctionClause {_clauseName = c ^. clauseName, ..}

instance HasExpressions Example where
  leafExpressions f = traverseOf exampleExpression (leafExpressions f)

instance HasExpressions FunctionDef where
  -- leafExpressions f (FunctionDef name ty clauses bi) = do
  leafExpressions f FunctionDef {..} = do
    clauses' <- traverse (leafExpressions f) _funDefClauses
    ty' <- leafExpressions f _funDefType
    examples' <- traverse (leafExpressions f) _funDefExamples
    pure
      FunctionDef
        { _funDefClauses = clauses',
          _funDefType = ty',
          _funDefExamples = examples',
          _funDefName,
          _funDefBuiltin
        }

instance HasExpressions InductiveParameter where
  leafExpressions _ param@InductiveParameter {} = do
    pure param

instance HasExpressions InductiveDef where
  leafExpressions f InductiveDef {..} = do
    params' <- traverse (leafExpressions f) _inductiveParameters
    constrs' <- traverse (leafExpressions f) _inductiveConstructors
    examples' <- traverse (leafExpressions f) _inductiveExamples
    pure
      InductiveDef
        { _inductiveParameters = params',
          _inductiveConstructors = constrs',
          _inductiveExamples = examples',
          _inductiveName,
          _inductiveBuiltin,
          _inductivePositive
        }

instance HasExpressions InductiveConstructorDef where
  -- leafExpressions f InductiveConstructorDef c args ret = do
  leafExpressions f InductiveConstructorDef {..} = do
    args' <- traverse (leafExpressions f) _inductiveConstructorParameters
    ret' <- leafExpressions f _inductiveConstructorReturnType
    examples' <- traverse (leafExpressions f) _inductiveConstructorExamples
    pure
      InductiveConstructorDef
        { _inductiveConstructorExamples = examples',
          _inductiveConstructorParameters = args',
          _inductiveConstructorReturnType = ret',
          _inductiveConstructorName
        }

fillHolesFunctionDef :: HashMap Hole Expression -> FunctionDef -> FunctionDef
fillHolesFunctionDef = subsHoles

fillHolesClause :: HashMap Hole Expression -> FunctionClause -> FunctionClause
fillHolesClause = subsHoles

fillHoles :: HashMap Hole Expression -> Expression -> Expression
fillHoles = subsHoles

substituteIndParams :: [(InductiveParameter, Expression)] -> Expression -> Expression
substituteIndParams = substitutionE . HashMap.fromList . map (first (^. inductiveParamName))

typeAbstraction :: IsImplicit -> Name -> FunctionParameter
typeAbstraction i var = FunctionParameter (Just var) i (ExpressionUniverse (SmallUniverse (getLoc var)))

mkFunction :: Expression -> Expression -> Function
mkFunction a = Function (unnamedParameter a)

unnamedParameter :: Expression -> FunctionParameter
unnamedParameter ty =
  FunctionParameter
    { _paramName = Nothing,
      _paramImplicit = Explicit,
      _paramType = ty
    }

renameToSubsE :: Rename -> SubsE
renameToSubsE = fmap (ExpressionIden . IdenVar)

patternArgVariables :: Traversal' PatternArg VarName
patternArgVariables f (PatternArg i n p) = PatternArg i <$> traverse f n <*> patternVariables f p

patternVariables :: Traversal' Pattern VarName
patternVariables f p = case p of
  PatternVariable v -> PatternVariable <$> f v
  PatternConstructorApp a -> PatternConstructorApp <$> goApp f a
  where
    goApp :: Traversal' ConstructorApp VarName
    goApp g = traverseOf constrAppParameters (traverse (patternArgVariables g))

inductiveTypeVarsAssoc :: (Foldable f) => InductiveDef -> f a -> HashMap VarName a
inductiveTypeVarsAssoc def l
  | length vars < n = impossible
  | otherwise = HashMap.fromList (zip vars (toList l))
  where
    n = length l
    vars :: [VarName]
    vars = def ^.. inductiveParameters . each . inductiveParamName

substitutionApp :: (Maybe Name, Expression) -> Expression -> Expression
substitutionApp (mv, ty) = case mv of
  Nothing -> id
  Just v -> substitutionE (HashMap.singleton v ty)

localsToSubsE :: LocalVars -> SubsE
localsToSubsE l = ExpressionIden . IdenVar <$> l ^. localTyMap

substitutionE :: SubsE -> Expression -> Expression
substitutionE m = over leafExpressions goLeaf
  where
    goLeaf :: Expression -> Expression
    goLeaf = \case
      ExpressionIden i -> goIden i
      e -> e
    goIden :: Iden -> Expression
    goIden i = case i of
      IdenVar v
        | Just e <- HashMap.lookup v m -> e
      _ -> ExpressionIden i

smallUniverseE :: Interval -> Expression
smallUniverseE = ExpressionUniverse . SmallUniverse

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [FunctionParameter] -> Expression -> Expression
foldFunType l r = go l
  where
    go :: [FunctionParameter] -> Expression
    go = \case
      [] -> r
      arg : args -> ExpressionFunction (Function arg (go args))

-- -- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Expression -> ([FunctionParameter], Expression)
unfoldFunType t = case t of
  ExpressionFunction (Function l r) -> first (l :) (unfoldFunType r)
  _ -> ([], t)

unfoldTypeAbsType :: Expression -> ([VarName], Expression)
unfoldTypeAbsType t = case t of
  ExpressionFunction (Function (FunctionParameter (Just var) _ _) r) ->
    first (var :) (unfoldTypeAbsType r)
  _ -> ([], t)

foldExplicitApplication :: Expression -> [Expression] -> Expression
foldExplicitApplication f = foldApplication f . zip (repeat Explicit)

foldApplication :: Expression -> [(IsImplicit, Expression)] -> Expression
foldApplication f = \case
  [] -> f
  ((i, a) : as) -> foldApplication (ExpressionApplication (Application f a i)) as

unfoldApplication' :: Application -> (Expression, NonEmpty (IsImplicit, Expression))
unfoldApplication' (Application l' r' i') = second (|: (i', r')) (unfoldExpressionApp l')

unfoldExpressionApp :: Expression -> (Expression, [(IsImplicit, Expression)])
unfoldExpressionApp = \case
  ExpressionApplication (Application l r i) ->
    second (`snoc` (i, r)) (unfoldExpressionApp l)
  e -> (e, [])

unfoldApplication :: Application -> (Expression, NonEmpty Expression)
unfoldApplication = fmap (fmap snd) . unfoldApplication'

reachableModules :: Module -> [Module]
reachableModules = fst . run . runOutputList . evalState (mempty :: HashSet Name) . go
  where
    go :: forall r. (Members '[State (HashSet Name), Output Module] r) => Module -> Sem r ()
    go m = do
      s <- get
      unless
        (HashSet.member (m ^. moduleName) s)
        (output m >> goBody (m ^. moduleBody))
      where
        goBody :: ModuleBody -> Sem r ()
        goBody = mapM_ goStatement . (^. moduleStatements)
        goStatement :: Statement -> Sem r ()
        goStatement = \case
          StatementInclude (Include inc) -> go inc
          _ -> return ()

(-->) :: Expression -> Expression -> Expression
(-->) a b =
  ExpressionFunction
    ( Function
        ( FunctionParameter
            { _paramName = Nothing,
              _paramImplicit = Explicit,
              _paramType = a
            }
        )
        b
    )
