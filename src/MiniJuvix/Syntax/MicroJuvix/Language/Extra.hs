module MiniJuvix.Syntax.MicroJuvix.Language.Extra
  ( module MiniJuvix.Syntax.MicroJuvix.Language.Extra,
    module MiniJuvix.Syntax.MicroJuvix.Language,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Language

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

newtype ConcreteType = ConcreteType {_unconcreteType :: Type}
  deriving stock (Eq, Generic)

type ConcreteTypeCall = TypeCall' ConcreteType

type TypeCall = TypeCall' Type

type SubsE = HashMap VarName Expression

type Rename = HashMap VarName VarName

type Subs = HashMap VarName Type

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

mkConcreteType' :: Type -> ConcreteType
mkConcreteType' =
  fromMaybe (error "the given type is not concrete")
    . mkConcreteType

mkConcreteType :: Type -> Maybe ConcreteType
mkConcreteType = fmap ConcreteType . go
  where
    go :: Type -> Maybe Type
    go t = case t of
      TypeApp (TypeApplication l r i) -> do
        l' <- go l
        r' <- go r
        return (TypeApp (TypeApplication l' r' i))
      TypeUniverse -> return TypeUniverse
      TypeFunction (Function l r) -> do
        l' <- go l
        r' <- go r
        return (TypeFunction (Function l' r'))
      TypeAbs {} -> Nothing
      TypeHole {} -> Nothing
      TypeIden i -> case i of
        TypeIdenInductive {} -> return t
        TypeIdenAxiom {} -> return t
        TypeIdenVariable {} -> Nothing

-- | unsafe version
expressionAsType' :: Expression -> Type
expressionAsType' = fromMaybe impossible . expressionAsType

findHoles :: Type -> HashSet Hole
findHoles = go
  where
    go :: Type -> HashSet Hole
    go = \case
      TypeIden {} -> mempty
      TypeApp (TypeApplication a b _) -> go a <> go b
      TypeFunction (Function a b) -> go a <> go b
      TypeAbs a -> go (a ^. typeAbsBody)
      TypeHole h -> HashSet.singleton h
      TypeUniverse -> mempty

hasHoles :: Type -> Bool
hasHoles = not . HashSet.null . findHoles

typeAsExpression :: Type -> Expression
typeAsExpression = go
  where
    go :: Type -> Expression
    go =
      \case
        TypeIden i -> ExpressionIden (goTypeIden i)
        TypeApp a -> ExpressionApplication (goApp a)
        TypeFunction f -> ExpressionFunction (goFunction f)
        TypeAbs {} -> error "TODO TypeAbs"
        TypeHole h -> ExpressionHole h
        TypeUniverse -> error "TODO TypeUniverse"

    goTypeIden :: TypeIden -> Iden
    goTypeIden = \case
      TypeIdenInductive i -> IdenInductive i
      TypeIdenAxiom a -> IdenAxiom a
      TypeIdenVariable v -> IdenVar v
    goApp :: TypeApplication -> Application
    goApp (TypeApplication l r i) = Application (go l) (go r) i
    goFunction :: Function -> FunctionExpression
    goFunction (Function l r) = FunctionExpression (go l) (go r)

fillHolesFunctionDef :: HashMap Hole Type -> FunctionDef -> FunctionDef
fillHolesFunctionDef m d =
  FunctionDef
    { _funDefName = d ^. funDefName,
      _funDefType = fillHolesType m (d ^. funDefType),
      _funDefClauses = fmap (fillHolesClause m) (d ^. funDefClauses),
      _funDefBuiltin = d ^. funDefBuiltin
    }

fillHolesClause :: HashMap Hole Type -> FunctionClause -> FunctionClause
fillHolesClause m = over clauseBody (fillHoles m)

fillHoles :: HashMap Hole Type -> Expression -> Expression
fillHoles m = goe
  where
    goe :: Expression -> Expression
    goe x = case x of
      ExpressionIden {} -> x
      ExpressionApplication a -> ExpressionApplication (goApp a)
      ExpressionLiteral {} -> x
      ExpressionHole h -> goHole h
      ExpressionFunction f -> ExpressionFunction (goFunction f)
      where
        goApp :: Application -> Application
        goApp (Application l r i) = Application (goe l) (goe r) i
        goFunction :: FunctionExpression -> FunctionExpression
        goFunction (FunctionExpression l r) = FunctionExpression (goe l) (goe r)
        goHole :: Hole -> Expression
        goHole h = case HashMap.lookup h m of
          Just r -> typeAsExpression r
          Nothing -> ExpressionHole h

fillHolesType :: HashMap Hole Type -> Type -> Type
fillHolesType m = go
  where
    go :: Type -> Type
    go = \case
      TypeIden i -> TypeIden i
      TypeApp a -> TypeApp (goApp a)
      TypeAbs a -> TypeAbs (goAbs a)
      TypeFunction f -> TypeFunction (goFunction f)
      TypeUniverse -> TypeUniverse
      TypeHole h -> goHole h
      where
        goApp :: TypeApplication -> TypeApplication
        goApp (TypeApplication l r i) = TypeApplication (go l) (go r) i
        goAbs :: TypeAbstraction -> TypeAbstraction
        goAbs (TypeAbstraction v i b) = TypeAbstraction v i (go b)
        goFunction :: Function -> Function
        goFunction (Function l r) = Function (go l) (go r)
        goHole :: Hole -> Type
        goHole h = case HashMap.lookup h m of
          Just ty -> ty
          Nothing -> TypeHole h

-- | If the expression is of type TypeUniverse it should return Just.
expressionAsType :: Expression -> Maybe Type
expressionAsType = go
  where
    go = \case
      ExpressionIden i -> TypeIden <$> goIden i
      ExpressionApplication a -> TypeApp <$> goApp a
      ExpressionLiteral {} -> Nothing
      ExpressionFunction f -> TypeFunction <$> goFunction f
      ExpressionHole h -> Just (TypeHole h)
    goFunction :: FunctionExpression -> Maybe Function
    goFunction (FunctionExpression l r) = do
      l' <- go l
      r' <- go r
      return (Function l' r')
    goIden :: Iden -> Maybe TypeIden
    goIden = \case
      IdenFunction {} -> Nothing
      IdenConstructor {} -> Nothing
      IdenVar v -> Just (TypeIdenVariable v)
      IdenAxiom a -> Just (TypeIdenAxiom a)
      IdenInductive i -> Just (TypeIdenInductive i)
    goApp :: Application -> Maybe TypeApplication
    goApp (Application l r i) = do
      l' <- go l
      r' <- go r
      return (TypeApplication l' r' i)

substituteIndParams :: [(InductiveParameter, Type)] -> Type -> Type
substituteIndParams = substitution . HashMap.fromList . map (first (^. inductiveParamName))

substitutionArg :: VarName -> VarName -> FunctionArgType -> FunctionArgType
substitutionArg from v a = case a of
  FunctionArgTypeAbstraction {} -> a
  FunctionArgTypeType ty ->
    FunctionArgTypeType
      (substituteType1 (from, TypeIden (TypeIdenVariable v)) ty)

renameToSubsE :: Rename -> SubsE
renameToSubsE = fmap (ExpressionIden . IdenVar)

renameExpression :: Rename -> Expression -> Expression
renameExpression r = substitutionE (renameToSubsE r)

substituteType1 :: (VarName, Type) -> Type -> Type
substituteType1 = substitution . uncurry HashMap.singleton

patternVariables :: Pattern -> [VarName]
patternVariables = \case
  PatternVariable v -> [v]
  PatternConstructorApp a -> goApp a
  PatternBraces b -> patternVariables b
  PatternWildcard {} -> []
  where
    goApp :: ConstructorApp -> [VarName]
    goApp (ConstructorApp _ ps) = concatMap patternVariables ps

renamePattern :: Rename -> Pattern -> Pattern
renamePattern m = go
  where
    go :: Pattern -> Pattern
    go p = case p of
      PatternVariable v
        | Just v' <- m ^. at v -> PatternVariable v'
      PatternConstructorApp a -> PatternConstructorApp (goApp a)
      _ -> p
    goApp :: ConstructorApp -> ConstructorApp
    goApp (ConstructorApp c ps) = ConstructorApp c (map go ps)

inductiveTypeVarsAssoc :: Foldable f => InductiveDef -> f a -> HashMap VarName a
inductiveTypeVarsAssoc def l
  | length vars < n = impossible
  | otherwise = HashMap.fromList (zip vars (toList l))
  where
    n = length l
    vars :: [VarName]
    vars = def ^.. inductiveParameters . each . inductiveParamName

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
            patternVar :: Pattern -> Maybe VarName
            patternVar = \case
              PatternVariable v -> Just v
              _ -> Nothing

substitutionConcrete :: ConcreteSubs -> Type -> ConcreteType
substitutionConcrete m = mkConcreteType' . substitution ((^. unconcreteType) <$> m)

concreteTypeToExpr :: ConcreteType -> Expression
concreteTypeToExpr = go . (^. unconcreteType)
  where
    go :: Type -> Expression
    go = \case
      TypeAbs {} -> impossible
      TypeIden i -> ExpressionIden (goIden i)
      TypeApp (TypeApplication l r i) -> ExpressionApplication (Application (go l) (go r) i)
      TypeFunction (Function l r) -> ExpressionFunction (FunctionExpression (go l) (go r))
      TypeUniverse {} -> impossible
      TypeHole {} -> impossible
    goIden :: TypeIden -> Iden
    goIden = \case
      TypeIdenInductive n -> IdenInductive n
      TypeIdenAxiom n -> IdenAxiom n
      TypeIdenVariable v -> IdenVar v

concreteSubsToSubsE :: ConcreteSubs -> SubsE
concreteSubsToSubsE = fmap concreteTypeToExpr

substitutionE :: SubsE -> Expression -> Expression
substitutionE m = go
  where
    go :: Expression -> Expression
    go x = case x of
      ExpressionIden i -> goIden i
      ExpressionApplication a -> ExpressionApplication (goApp a)
      ExpressionLiteral {} -> x
      ExpressionHole {} -> x
      ExpressionFunction f -> ExpressionFunction (goFunction f)
    goApp :: Application -> Application
    goApp (Application l r i) = Application (go l) (go r) i
    goFunction :: FunctionExpression -> FunctionExpression
    goFunction (FunctionExpression l r) = FunctionExpression (go l) (go r)
    goIden :: Iden -> Expression
    goIden i = case i of
      IdenVar v
        | Just e <- HashMap.lookup v m -> e
      _ -> ExpressionIden i

substitution :: Subs -> Type -> Type
substitution m = go
  where
    go :: Type -> Type
    go = \case
      TypeIden i -> goIden i
      TypeApp a -> TypeApp (goApp a)
      TypeAbs a -> TypeAbs (goAbs a)
      TypeFunction f -> TypeFunction (goFunction f)
      TypeUniverse -> TypeUniverse
      TypeHole h -> TypeHole h

    goApp :: TypeApplication -> TypeApplication
    goApp (TypeApplication l r i) = TypeApplication (go l) (go r) i

    goAbs :: TypeAbstraction -> TypeAbstraction
    goAbs (TypeAbstraction v i b) = TypeAbstraction v i (go b)

    goFunction :: Function -> Function
    goFunction (Function l r) = Function (go l) (go r)

    goIden :: TypeIden -> Type
    goIden i = case i of
      TypeIdenInductive {} -> TypeIden i
      TypeIdenAxiom {} -> TypeIden i
      TypeIdenVariable v -> case HashMap.lookup v m of
        Just ty -> ty
        Nothing -> TypeIden i

-- | [a, b] c ==> a -> (b -> c)
foldFunType :: [FunctionArgType] -> Type -> Type
foldFunType l r = case l of
  [] -> r
  (a : as) ->
    let r' = foldFunType as r
     in case a of
          FunctionArgTypeAbstraction (i, v) -> TypeAbs (TypeAbstraction v i r')
          FunctionArgTypeType t -> TypeFunction (Function t r')

-- | a -> (b -> c)  ==> ([a, b], c)
unfoldFunType :: Type -> ([FunctionArgType], Type)
unfoldFunType t = case t of
  TypeFunction (Function l r) -> first (FunctionArgTypeType l :) (unfoldFunType r)
  TypeAbs (TypeAbstraction var i r) -> first (FunctionArgTypeAbstraction (i, var) :) (unfoldFunType r)
  _ -> ([], t)

unfoldFunConcreteType :: ConcreteType -> ([ConcreteType], ConcreteType)
unfoldFunConcreteType = bimap (map mkConcreteType') mkConcreteType' . go . (^. unconcreteType)
  where
    go :: Type -> ([Type], Type)
    go t = case t of
      TypeFunction (Function l r) -> first (l :) (go r)
      _ -> ([], t)

unfoldTypeAbsType :: Type -> ([VarName], Type)
unfoldTypeAbsType t = case t of
  TypeAbs (TypeAbstraction var _ r) -> first (var :) (unfoldTypeAbsType r)
  _ -> ([], t)

foldExplicitApplication :: Expression -> [Expression] -> Expression
foldExplicitApplication f = foldApplication f . zip (repeat Explicit)

foldApplication :: Expression -> [(IsImplicit, Expression)] -> Expression
foldApplication f = \case
  [] -> f
  ((i, a) : as) -> foldApplication (ExpressionApplication (Application f a i)) as

unfoldApplication' :: Application -> (Expression, NonEmpty (IsImplicit, Expression))
unfoldApplication' (Application l' r' i') = second (|: (i', r')) (unfoldExpression l')
  where
    unfoldExpression :: Expression -> (Expression, [(IsImplicit, Expression)])
    unfoldExpression e = case e of
      ExpressionApplication (Application l r i) ->
        second (`snoc` (i, r)) (unfoldExpression l)
      _ -> (e, [])

unfoldApplication :: Application -> (Expression, NonEmpty Expression)
unfoldApplication = fmap (fmap snd) . unfoldApplication'

unfoldTypeApplication :: TypeApplication -> (Type, NonEmpty Type)
unfoldTypeApplication (TypeApplication l' r' _) = second (|: r') (unfoldType l')

unfoldType :: Type -> (Type, [Type])
unfoldType = \case
  TypeApp (TypeApplication l r _) -> second (`snoc` r) (unfoldType l)
  t -> (t, [])

foldTypeApp :: Type -> [Type] -> Type
foldTypeApp ty = \case
  [] -> ty
  (p : ps) -> foldTypeApp (TypeApp (TypeApplication ty p Explicit)) ps

foldTypeAppName :: Name -> [Name] -> Type
foldTypeAppName tyName indParams =
  foldTypeApp
    (TypeIden (TypeIdenInductive tyName))
    (map (TypeIden . TypeIdenVariable) indParams)

getTypeName :: Type -> Maybe Name
getTypeName = \case
  (TypeIden (TypeIdenInductive tyName)) -> Just tyName
  (TypeApp (TypeApplication l _ _)) -> getTypeName l
  _ -> Nothing

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
