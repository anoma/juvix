module Juvix.Syntax.MicroJuvix.Language.Extra
  ( module Juvix.Syntax.MicroJuvix.Language.Extra,
    module Juvix.Syntax.MicroJuvix.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Prelude
import Juvix.Syntax.MicroJuvix.Language
import Juvix.Syntax.MicroJuvix.LocalVars

data Caller
  = CallerInductive InductiveName
  | CallerFunction FunctionName
  | CallerAxiom AxiomName
  deriving stock (Eq, Ord, Generic)

data TypeCallIden
  = InductiveIden InductiveName
  | FunctionIden FunctionName
  deriving stock (Eq, Ord, Generic)

data TypeCall' a = TypeCall'
  { _typeCallIden :: TypeCallIden,
    _typeCallArguments :: NonEmpty a
  }
  deriving stock (Eq, Generic)

newtype TypeCallsMap = TypeCallsMap
  { _typeCallsMap :: HashMap Caller (HashSet TypeCall)
  }

instance Functor TypeCall' where
  fmap f (TypeCall' i args) = TypeCall' i (fmap f args)

newtype ConcreteType = ConcreteType {_unconcreteType :: Expression}
  deriving stock (Eq, Generic)

type ConcreteTypeCall = TypeCall' ConcreteType

type TypeCall = TypeCall' Expression

type SubsE = HashMap VarName Expression

type Rename = HashMap VarName VarName

type Subs = HashMap VarName Expression

type ConcreteSubs = HashMap VarName ConcreteType

-- | Indexed by _typeCallIden
newtype TypeCalls = TypeCalls
  { _typeCallSet :: HashMap TypeCallIden (HashMap ConcreteTypeCall ConcreteSubs)
  }

type VarMap = HashMap VarName VarName

emptyCalls :: TypeCalls
emptyCalls = TypeCalls mempty

instance Hashable TypeCallIden

instance Hashable TypeCall

instance Hashable Caller

instance Hashable ConcreteTypeCall

instance Hashable ConcreteType

makeLenses ''TypeCalls
makeLenses ''TypeCall'
makeLenses ''TypeCallsMap
makeLenses ''ConcreteType

typeCallIdenToCaller :: TypeCallIden -> Caller
typeCallIdenToCaller = \case
  InductiveIden i -> CallerInductive i
  FunctionIden i -> CallerFunction i

mkConcreteType' :: Expression -> ConcreteType
mkConcreteType' =
  fromMaybe (error "the given type is not concrete")
    . mkConcreteType

-- TODO: consider using traversals to simplify
mkConcreteType :: Expression -> Maybe ConcreteType
mkConcreteType = fmap ConcreteType . go
  where
    go :: Expression -> Maybe Expression
    go t = case t of
      ExpressionApplication (Application l r i) -> do
        l' <- go l
        r' <- go r
        return (ExpressionApplication (Application l' r' i))
      ExpressionUniverse {} -> return t
      ExpressionLambda (Lambda v ty b) -> do
        b' <- go b
        ty' <- go ty
        return (ExpressionLambda (Lambda v ty' b'))
      ExpressionFunction (Function l r) -> do
        l' <- goParam l
        r' <- go r
        return (ExpressionFunction (Function l' r'))
      ExpressionHole {} -> Nothing
      ExpressionLiteral {} -> return t
      ExpressionIden i -> case i of
        IdenFunction {} -> return t
        IdenInductive {} -> return t
        IdenConstructor {} -> return t
        IdenAxiom {} -> return t
        IdenVar {} -> Nothing
    goParam :: FunctionParameter -> Maybe FunctionParameter
    goParam (FunctionParameter m i e) = do
      guard (isNothing m)
      e' <- go e
      return (FunctionParameter m i e')

newtype PolyType = PolyType {_unpolyType :: Expression}
  deriving stock (Eq, Generic)

instance Hashable PolyType

makeLenses ''PolyType

mkPolyType' :: Expression -> PolyType
mkPolyType' =
  fromMaybe (error "the given type contains holes")
    . mkPolyType

-- mkPolyType removes all named function parameters; currently the assumption in
-- MicroJuvixToMiniC.hs is that these coincide with type parameters
mkPolyType :: Expression -> Maybe PolyType
mkPolyType = fmap PolyType . go
  where
    go :: Expression -> Maybe Expression
    go t = case t of
      ExpressionApplication (Application l r i) -> do
        l' <- go l
        r' <- go r
        return (ExpressionApplication (Application l' r' i))
      ExpressionUniverse {} -> return t
      ExpressionFunction (Function (FunctionParameter m i e) r)
        | isNothing m -> do
            e' <- go e
            r' <- go r
            return (ExpressionFunction (Function (FunctionParameter m i e') r'))
        | otherwise -> go r
      ExpressionHole {} -> Nothing
      ExpressionLiteral {} -> return t
      ExpressionIden IdenFunction {} -> return t
      ExpressionIden IdenInductive {} -> return t
      ExpressionIden IdenConstructor {} -> return t
      ExpressionIden IdenAxiom {} -> return t
      ExpressionIden IdenVar {} -> return t
      ExpressionLambda (Lambda v ty b) -> do
        b' <- go b
        ty' <- go ty
        return (ExpressionLambda (Lambda v ty' b'))

class HasExpressions a where
  leafExpressions :: Traversal' a Expression

instance HasExpressions Expression where
  leafExpressions f e = case e of
    ExpressionIden {} -> f e
    ExpressionApplication a -> ExpressionApplication <$> leafExpressions f a
    ExpressionFunction fun -> ExpressionFunction <$> leafExpressions f fun
    ExpressionLambda l -> ExpressionLambda <$> leafExpressions f l
    ExpressionLiteral {} -> f e
    ExpressionUniverse {} -> f e
    ExpressionHole {} -> f e

instance HasExpressions Lambda where
  leafExpressions f (Lambda v ty b) = do
    b' <- leafExpressions f b
    ty' <- leafExpressions f ty
    pure (Lambda v ty' b')

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
  ExpressionIden {} -> pure e
  ExpressionApplication {} -> pure e
  ExpressionFunction {} -> pure e
  ExpressionLiteral {} -> pure e
  ExpressionUniverse {} -> pure e
  ExpressionHole h -> ExpressionHole <$> f h
  ExpressionLambda {} -> pure e

holes :: HasExpressions a => Traversal' a Hole
holes = leafExpressions . _ExpressionHole

hasHoles :: HasExpressions a => a -> Bool
hasHoles = has holes

subsHoles :: HasExpressions a => HashMap Hole Expression -> a -> a
subsHoles s = over leafExpressions helper
  where
    helper :: Expression -> Expression
    helper e = case e of
      ExpressionHole h -> fromMaybe e (s ^. at h)
      _ -> e

instance HasExpressions FunctionClause where
  leafExpressions f (FunctionClause n ps b) = do
    b' <- leafExpressions f b
    pure (FunctionClause n ps b')

instance HasExpressions FunctionDef where
  leafExpressions f (FunctionDef name ty clauses bi) = do
    clauses' <- traverse (leafExpressions f) clauses
    ty' <- leafExpressions f ty
    pure (FunctionDef name ty' clauses' bi)

instance HasExpressions InductiveParameter where
  leafExpressions _ param@(InductiveParameter _) = do
    pure param

instance HasExpressions InductiveDef where
  leafExpressions f (InductiveDef name built params constrs pos) = do
    params' <- traverse (leafExpressions f) params
    constrs' <- traverse (leafExpressions f) constrs
    pure (InductiveDef name built params' constrs' pos)

instance HasExpressions InductiveConstructorDef where
  leafExpressions f (InductiveConstructorDef c args ret) = do
    args' <- traverse (leafExpressions f) args
    ret' <- leafExpressions f ret
    pure (InductiveConstructorDef c args' ret')

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

unnamedParameter :: Expression -> FunctionParameter
unnamedParameter ty =
  FunctionParameter
    { _paramName = Nothing,
      _paramImplicit = Explicit,
      _paramType = ty
    }

renameToSubsE :: Rename -> SubsE
renameToSubsE = fmap (ExpressionIden . IdenVar)

renameExpression :: Rename -> Expression -> Expression
renameExpression r = substitutionE (renameToSubsE r)

patternArgVariables :: Traversal' PatternArg VarName
patternArgVariables f = traverseOf patternArgPattern (patternVariables f)

patternVariables :: Traversal' Pattern VarName
patternVariables f p = case p of
  PatternVariable v -> PatternVariable <$> f v
  PatternConstructorApp a -> PatternConstructorApp <$> goApp f a
  PatternWildcard {} -> pure p
  where
    goApp :: Traversal' ConstructorApp VarName
    goApp g = traverseOf constrAppParameters (traverse (patternArgVariables g))

renamePatternArg :: Rename -> PatternArg -> PatternArg
renamePatternArg = over patternArgPattern . renamePattern

renamePattern :: Rename -> Pattern -> Pattern
renamePattern m = over patternVariables renameVar
  where
    renameVar :: VarName -> VarName
    renameVar v
      | Just v' <- m ^. at v = v'
      | otherwise = v

inductiveTypeVarsAssoc :: Foldable f => InductiveDef -> f a -> HashMap VarName a
inductiveTypeVarsAssoc def l
  | length vars < n = impossible
  | otherwise = HashMap.fromList (zip vars (toList l))
  where
    n = length l
    vars :: [VarName]
    vars = def ^.. inductiveParameters . each . inductiveParamName

-- TODO remove this after monojuvix is gone
functionTypeVarsAssoc :: forall a f. Foldable f => FunctionDef -> f a -> HashMap VarName a
functionTypeVarsAssoc def l = sig <> mconcatMap clause (def ^. funDefClauses)
  where
    n = length l
    zipl :: [Maybe VarName] -> HashMap VarName a
    zipl x = HashMap.fromList (mapMaybe aux (zip x (toList l)))
      where
        aux :: (Maybe x, y) -> Maybe (x, y)
        aux = \case
          (Just a, b) -> Just (a, b)
          _ -> Nothing
    sig
      | length tyVars < n = impossible
      | otherwise = zipl (map Just tyVars)
      where
        tyVars = fst (unfoldTypeAbsType (def ^. funDefType))
    clause :: FunctionClause -> HashMap VarName a
    clause c = zipl clauseVars
      where
        clauseVars :: [Maybe VarName]
        clauseVars = take n (map patternVar (c ^. clausePatterns))
          where
            patternVar :: PatternArg -> Maybe VarName
            patternVar a = case a ^. patternArgPattern of
              PatternVariable v -> Just v
              _ -> Nothing

substitutionConcrete :: ConcreteSubs -> Expression -> ConcreteType
substitutionConcrete m = mkConcreteType' . substitutionE ((^. unconcreteType) <$> m)

concreteTypeToExpr :: ConcreteType -> Expression
concreteTypeToExpr = (^. unconcreteType)

concreteSubsToSubsE :: ConcreteSubs -> SubsE
concreteSubsToSubsE = fmap concreteTypeToExpr

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

smallUniverse :: Interval -> Expression
smallUniverse = ExpressionUniverse . SmallUniverse

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
    go :: forall r. Members '[State (HashSet Name), Output Module] r => Module -> Sem r ()
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
