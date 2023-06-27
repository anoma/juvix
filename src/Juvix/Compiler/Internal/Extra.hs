module Juvix.Compiler.Internal.Extra
  ( module Juvix.Compiler.Internal.Extra,
    module Juvix.Compiler.Internal.Language,
  )
where

import Data.Generics.Uniplate.Data hiding (holes)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Internal.Data.LocalVars
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

type SubsE = HashMap VarName Expression

type Rename = HashMap VarName VarName

type Subs = HashMap VarName Expression

type VarMap = HashMap VarName VarName

data ApplicationArg = ApplicationArg
  { _appArgIsImplicit :: IsImplicit,
    _appArg :: Expression
  }

makeLenses ''ApplicationArg

instance HasLoc ApplicationArg where
  getLoc = getLoc . (^. appArg)

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
  leafExpressions f a = do
    let _constrAppConstructor = a ^. constrAppConstructor
    _constrAppType <- traverseOf _Just (leafExpressions f) (a ^. constrAppType)
    _constrAppParameters <- traverseOf each (leafExpressions f) (a ^. constrAppParameters)
    pure ConstructorApp {..}

instance HasExpressions PatternArg where
  leafExpressions f a = do
    let _patternArgIsImplicit = a ^. patternArgIsImplicit
        _patternArgName = a ^. patternArgName
    _patternArgPattern <- leafExpressions f (a ^. patternArgPattern)
    pure PatternArg {..}

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

instance HasExpressions MutualBlockLet where
  leafExpressions f (MutualBlockLet defs) =
    MutualBlockLet <$> traverse (leafExpressions f) defs

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
  leafExpressions f a = do
    _typedExpression <- leafExpressions f (a ^. typedExpression)
    _typedType <- leafExpressions f (a ^. typedType)
    pure TypedExpression {..}

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
          _funDefTerminating,
          _funDefName,
          _funDefBuiltin,
          _funDefPragmas
        }

instance HasExpressions MutualStatement where
  leafExpressions f = \case
    StatementFunction d -> StatementFunction <$> leafExpressions f d
    StatementInductive d -> StatementInductive <$> leafExpressions f d

instance HasExpressions InductiveParameter where
  leafExpressions _ param@InductiveParameter {} = do
    pure param

instance HasExpressions InductiveDef where
  leafExpressions f InductiveDef {..} = do
    params' <- traverse (leafExpressions f) _inductiveParameters
    constrs' <- traverse (leafExpressions f) _inductiveConstructors
    examples' <- traverse (leafExpressions f) _inductiveExamples
    ty' <- leafExpressions f _inductiveType
    pure
      InductiveDef
        { _inductiveParameters = params',
          _inductiveConstructors = constrs',
          _inductiveExamples = examples',
          _inductiveType = ty',
          _inductiveName,
          _inductiveBuiltin,
          _inductivePositive,
          _inductivePragmas
        }

instance HasExpressions InductiveConstructorDef where
  leafExpressions f InductiveConstructorDef {..} = do
    ty' <- leafExpressions f _inductiveConstructorType
    examples' <- traverse (leafExpressions f) _inductiveConstructorExamples
    pure
      InductiveConstructorDef
        { _inductiveConstructorExamples = examples',
          _inductiveConstructorType = ty',
          _inductiveConstructorName,
          _inductiveConstructorPragmas
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

foldFunTypeExplicit :: [Expression] -> Expression -> Expression
foldFunTypeExplicit = foldFunType . map unnamedParameter

viewConstructorType :: Expression -> ([Expression], Expression)
viewConstructorType = first (map (^. paramType)) . unfoldFunType

constructorArgs :: Expression -> [Expression]
constructorArgs = fst . viewConstructorType

-- | a -> (b -> c)  ==> ([a, b], c)
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
foldExplicitApplication f = foldApplication f . map (ApplicationArg Explicit)

foldApplication :: Expression -> [ApplicationArg] -> Expression
foldApplication f args = case args of
  [] -> f
  ((ApplicationArg i a) : as) -> foldApplication (ExpressionApplication (Application f a i)) as

unfoldApplication' :: Application -> (Expression, NonEmpty ApplicationArg)
unfoldApplication' (Application l' r' i') = second (|: (ApplicationArg i' r')) (unfoldExpressionApp l')

unfoldExpressionApp :: Expression -> (Expression, [ApplicationArg])
unfoldExpressionApp = \case
  ExpressionApplication (Application l r i) ->
    second (`snoc` ApplicationArg i r) (unfoldExpressionApp l)
  e -> (e, [])

unfoldApplication :: Application -> (Expression, NonEmpty Expression)
unfoldApplication = fmap (fmap (^. appArg)) . unfoldApplication'

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
        goBody = mapM_ (go . (^. includeModule)) . (^. moduleIncludes)

-- | A fold over all transitive children, including self
patternCosmos :: SimpleFold Pattern Pattern
patternCosmos f p = case p of
  PatternVariable {} -> f p
  PatternConstructorApp ConstructorApp {..} ->
    f p *> do
      args' <- traverse (traverseOf patternArgPattern (patternCosmos f)) _constrAppParameters
      pure $
        PatternConstructorApp
          ConstructorApp
            { _constrAppParameters = args',
              _constrAppConstructor,
              _constrAppType
            }

patternArgNameFold :: SimpleFold (Maybe Name) Pattern
patternArgNameFold f = \case
  Nothing -> mempty
  Just n -> Const (getConst (f (PatternVariable n)))

-- | A fold over all transitive children, including self
patternArgCosmos :: SimpleFold PatternArg Pattern
patternArgCosmos f p = do
  _patternArgPattern <- patternCosmos f (p ^. patternArgPattern)
  _patternArgName <- patternArgNameFold f (p ^. patternArgName)
  pure PatternArg {..}
  where
    _patternArgIsImplicit = p ^. patternArgIsImplicit

-- | A fold over all transitive children, excluding self
patternSubCosmos :: SimpleFold Pattern Pattern
patternSubCosmos f p = case p of
  PatternVariable {} -> pure p
  PatternConstructorApp ConstructorApp {..} -> do
    args' <- traverse (patternArgCosmos f) _constrAppParameters
    pure $
      PatternConstructorApp
        ConstructorApp
          { _constrAppParameters = args',
            _constrAppConstructor,
            _constrAppType
          }

viewAppArgAsPattern :: ApplicationArg -> Maybe PatternArg
viewAppArgAsPattern a = do
  p' <- viewExpressionAsPattern (a ^. appArg)
  return
    ( PatternArg
        { _patternArgIsImplicit = a ^. appArgIsImplicit,
          _patternArgName = Nothing,
          _patternArgPattern = p'
        }
    )

viewApp :: Expression -> (Expression, [ApplicationArg])
viewApp e =
  case e of
    ExpressionApplication (Application l r i) ->
      second (`snoc` ApplicationArg i r) (viewApp l)
    _ -> (e, [])

viewExpressionAsPattern :: Expression -> Maybe Pattern
viewExpressionAsPattern e = case viewApp e of
  (f, args)
    | Just c <- getConstructor f -> do
        args' <- mapM viewAppArgAsPattern args
        Just $ PatternConstructorApp (ConstructorApp c args' Nothing)
  (f, [])
    | Just v <- getVariable f -> Just (PatternVariable v)
  _ -> Nothing
  where
    getConstructor :: Expression -> Maybe ConstructorName
    getConstructor f = case f of
      ExpressionIden (IdenConstructor n) -> Just n
      _ -> Nothing
    getVariable :: Expression -> Maybe VarName
    getVariable f = case f of
      ExpressionIden (IdenVar n) -> Just n
      _ -> Nothing

class IsExpression a where
  toExpression :: a -> Expression

instance IsExpression Iden where
  toExpression = ExpressionIden

instance IsExpression Expression where
  toExpression = id

instance IsExpression Hole where
  toExpression = ExpressionHole

instance IsExpression Name where
  toExpression n = ExpressionIden (mkIden n)
    where
      mkIden = case n ^. nameKind of
        KNameConstructor -> IdenConstructor
        KNameInductive -> IdenInductive
        KNameFunction -> IdenFunction
        KNameAxiom -> IdenAxiom
        KNameLocal -> IdenVar
        KNameLocalModule -> impossible
        KNameTopModule -> impossible

instance IsExpression SmallUniverse where
  toExpression = ExpressionUniverse

instance IsExpression Application where
  toExpression = ExpressionApplication

instance IsExpression Function where
  toExpression = ExpressionFunction

instance IsExpression ConstructorApp where
  toExpression (ConstructorApp c args _) =
    foldApplication (toExpression c) (map toApplicationArg args)

toApplicationArg :: PatternArg -> ApplicationArg
toApplicationArg p =
  set appArgIsImplicit (p ^. patternArgIsImplicit) (helper (p ^. patternArgPattern))
  where
    helper :: Pattern -> ApplicationArg
    helper = \case
      PatternVariable v -> ApplicationArg Explicit (toExpression v)
      PatternConstructorApp a -> ApplicationArg Explicit (toExpression a)

expressionArrow :: (IsExpression a, IsExpression b) => IsImplicit -> a -> b -> Expression
expressionArrow isImpl a b =
  ExpressionFunction
    ( Function
        ( FunctionParameter
            { _paramName = Nothing,
              _paramImplicit = isImpl,
              _paramType = toExpression a
            }
        )
        (toExpression b)
    )

infixr 0 <>-->

(<>-->) :: (IsExpression a, IsExpression b) => a -> b -> Expression
(<>-->) = expressionArrow Implicit

infixr 0 -->

(-->) :: (IsExpression a, IsExpression b) => a -> b -> Expression
(-->) = expressionArrow Explicit

infix 4 ===

(===) :: (IsExpression a, IsExpression b) => a -> b -> Bool
a === b = (toExpression a ==% toExpression b) mempty

leftEq :: (IsExpression a, IsExpression b) => a -> b -> HashSet Name -> Bool
leftEq a b free =
  isRight
    . run
    . runError @Text
    . runReader free
    . evalState (mempty @(HashMap Name Name))
    $ matchExpressions (toExpression a) (toExpression b)

clauseLhsAsExpression :: FunctionClause -> Expression
clauseLhsAsExpression cl =
  foldApplication (toExpression (cl ^. clauseName)) (map toApplicationArg (cl ^. clausePatterns))

infix 4 ==%

(==%) :: (IsExpression a, IsExpression b) => a -> b -> HashSet Name -> Bool
(==%) a b free = leftEq a b free || leftEq b a free

infixl 9 @@

(@@) :: (IsExpression a, IsExpression b) => a -> b -> Expression
a @@ b = toExpression (Application (toExpression a) (toExpression b) Explicit)

freshVar :: (Member NameIdGen r) => Text -> Sem r VarName
freshVar n = do
  uid <- freshNameId
  return
    Name
      { _nameId = uid,
        _nameText = n,
        _nameKind = KNameLocal,
        _namePretty = n,
        _nameFixity = Nothing,
        _nameLoc = error "freshVar with no location"
      }

freshHole :: Members '[NameIdGen] r => Interval -> Sem r Hole
freshHole l = mkHole l <$> freshNameId

mkFreshHole :: Members '[NameIdGen] r => Interval -> Sem r Expression
mkFreshHole l = ExpressionHole <$> freshHole l

matchExpressions ::
  forall r.
  (Members '[State (HashMap Name Name), Reader (HashSet VarName), Error Text] r) =>
  Expression ->
  Expression ->
  Sem r ()
matchExpressions = go
  where
    -- Soft free vars are allowed to be matched
    isSoftFreeVar :: VarName -> Sem r Bool
    isSoftFreeVar = asks . HashSet.member
    go :: Expression -> Expression -> Sem r ()
    go a b = case (a, b) of
      (ExpressionIden ia, ExpressionIden ib) -> case (ia, ib) of
        (IdenVar va, IdenVar vb) -> do
          addIfFreeVar va vb
          addIfFreeVar vb va
          unlessM (matchVars va vb) err
        (_, _) -> unless (ia == ib) err
      (ExpressionIden (IdenVar va), ExpressionHole h) -> goHole va h
      (ExpressionHole h, ExpressionIden (IdenVar vb)) -> goHole vb h
      (ExpressionIden {}, _) -> err
      (_, ExpressionIden {}) -> err
      (ExpressionApplication ia, ExpressionApplication ib) ->
        goApp ia ib
      (ExpressionApplication {}, _) -> err
      (_, ExpressionApplication {}) -> err
      (ExpressionLambda ia, ExpressionLambda ib) ->
        goLambda ia ib
      (ExpressionLambda {}, _) -> err
      (_, ExpressionLambda {}) -> err
      (ExpressionCase {}, ExpressionCase {}) -> error "not implemented"
      (ExpressionCase {}, _) -> err
      (_, ExpressionCase {}) -> err
      (ExpressionUniverse ia, ExpressionUniverse ib) ->
        unless (ia == ib) err
      (ExpressionUniverse {}, _) -> err
      (_, ExpressionUniverse {}) -> err
      (ExpressionFunction ia, ExpressionFunction ib) ->
        goFunction ia ib
      (ExpressionFunction {}, _) -> err
      (_, ExpressionFunction {}) -> err
      (ExpressionSimpleLambda {}, ExpressionSimpleLambda {}) -> error "not implemented"
      (ExpressionSimpleLambda {}, _) -> err
      (_, ExpressionSimpleLambda {}) -> err
      (ExpressionLiteral ia, ExpressionLiteral ib) ->
        unless (ia == ib) err
      (ExpressionLiteral {}, _) -> err
      (_, ExpressionLiteral {}) -> err
      (ExpressionLet {}, ExpressionLet {}) -> error "not implemented"
      (_, ExpressionLet {}) -> err
      (ExpressionLet {}, _) -> err
      (ExpressionHole _, ExpressionHole _) -> return ()

    err :: Sem r a
    err = throw @Text "Expression missmatch"

    matchVars :: Name -> Name -> Sem r Bool
    matchVars va vb = (== Just vb) <$> gets @(HashMap Name Name) (^. at va)

    goHole :: Name -> Hole -> Sem r ()
    goHole var h = do
      whenM (gets @(HashMap Name Name) (HashMap.member var)) err
      let vh = varFromHole h
      addName var vh

    addIfFreeVar :: VarName -> VarName -> Sem r ()
    addIfFreeVar va vb = whenM (isSoftFreeVar va) (addName va vb)

    goLambda :: Lambda -> Lambda -> Sem r ()
    goLambda = error "TODO not implemented yet"

    goApp :: Application -> Application -> Sem r ()
    goApp (Application al ar aim) (Application bl br bim) = do
      unless (aim == bim) err
      go al bl
      go ar br

    goFunction :: Function -> Function -> Sem r ()
    goFunction (Function al ar) (Function bl br) = do
      matchFunctionParameter al bl
      matchExpressions ar br

addName :: (Member (State (HashMap Name Name)) r) => Name -> Name -> Sem r ()
addName na nb = modify (HashMap.insert na nb)

matchFunctionParameter ::
  forall r.
  (Members '[State (HashMap Name Name), Reader (HashSet VarName), Error Text] r) =>
  FunctionParameter ->
  FunctionParameter ->
  Sem r ()
matchFunctionParameter pa pb = do
  goParamName (pa ^. paramName) (pb ^. paramName)
  goParamImplicit (pa ^. paramImplicit) (pb ^. paramImplicit)
  goParamType (pa ^. paramType) (pb ^. paramType)
  where
    goParamType :: Expression -> Expression -> Sem r ()
    goParamType ua ub = matchExpressions ua ub
    goParamImplicit :: IsImplicit -> IsImplicit -> Sem r ()
    goParamImplicit ua ub = unless (ua == ub) (throw @Text "implicit mismatch")
    goParamName :: Maybe VarName -> Maybe VarName -> Sem r ()
    goParamName (Just va) (Just vb) = addName va vb
    goParamName _ _ = return ()

isSmallUniverse' :: Expression -> Bool
isSmallUniverse' = \case
  ExpressionUniverse {} -> True
  _ -> False

allTypeSignatures :: Data a => a -> [Expression]
allTypeSignatures a =
  [f ^. funDefType | f@FunctionDef {} <- universeBi a]
    <> [f ^. axiomType | f@AxiomDef {} <- universeBi a]
    <> [f ^. inductiveType | f@InductiveDef {} <- universeBi a]

idenName :: Iden -> Name
idenName = \case
  IdenFunction f -> f
  IdenConstructor c -> c
  IdenVar v -> v
  IdenInductive i -> i
  IdenAxiom a -> a
