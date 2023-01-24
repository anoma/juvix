module Juvix.Compiler.Abstract.Extra.Functions where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Juvix.Compiler.Abstract.Language
import Juvix.Data.Effect.NameIdGen
import Juvix.Prelude

data ApplicationArg = ApplicationArg
  { _appArgIsImplicit :: IsImplicit,
    _appArg :: Expression
  }

makeLenses ''ApplicationArg

patternVariables :: Traversal' Pattern VarName
patternVariables f p = case p of
  PatternVariable v -> PatternVariable <$> f v
  PatternWildcard {} -> pure p
  PatternEmpty {} -> pure p
  PatternConstructorApp app -> PatternConstructorApp <$> appVariables f app

patternArgVariables :: Traversal' PatternArg VarName
patternArgVariables f (PatternArg i n p) = PatternArg i <$> traverse f n <*> patternVariables f p

appVariables :: Traversal' ConstructorApp VarName
appVariables f = traverseOf constrAppParameters (traverse (patternArgVariables f))

idenName :: Iden -> Name
idenName = \case
  IdenFunction (FunctionRef f) -> f
  IdenConstructor (ConstructorRef c) -> c
  IdenVar v -> v
  IdenInductive (InductiveRef i) -> i
  IdenAxiom (AxiomRef a) -> a

-- | A fold over all transitive children, including self
patternCosmos :: SimpleFold Pattern Pattern
patternCosmos f p = case p of
  PatternVariable {} -> f p
  PatternWildcard {} -> f p
  PatternEmpty {} -> f p
  PatternConstructorApp (ConstructorApp r args) ->
    f p *> do
      args' <- traverse (traverseOf patternArgPattern (patternCosmos f)) args
      pure (PatternConstructorApp (ConstructorApp r args'))

-- | A fold over all transitive children, excluding self
patternSubCosmos :: SimpleFold Pattern Pattern
patternSubCosmos f p = case p of
  PatternVariable {} -> pure p
  PatternWildcard {} -> pure p
  PatternEmpty {} -> pure p
  PatternConstructorApp (ConstructorApp r args) -> do
    args' <- traverse (traverseOf patternArgPattern (patternCosmos f)) args
    pure (PatternConstructorApp (ConstructorApp r args'))

viewApp :: Expression -> (Expression, [ApplicationArg])
viewApp e =
  case e of
    ExpressionApplication (Application l r i) ->
      second (`snoc` ApplicationArg i r) (viewApp l)
    _ -> (e, [])

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

viewExpressionAsPattern :: Expression -> Maybe Pattern
viewExpressionAsPattern e = case viewApp e of
  (f, args)
    | Just c <- getConstructor f -> do
        args' <- mapM viewAppArgAsPattern args
        Just $ PatternConstructorApp (ConstructorApp c args')
  (f, [])
    | Just v <- getVariable f -> Just (PatternVariable v)
  _ -> Nothing
  where
    getConstructor :: Expression -> Maybe ConstructorRef
    getConstructor f = case f of
      ExpressionIden (IdenConstructor n) -> Just n
      _ -> Nothing
    getVariable :: Expression -> Maybe VarName
    getVariable f = case f of
      ExpressionIden (IdenVar n) -> Just n
      _ -> Nothing

addName :: (Member (State (HashMap Name Name)) r) => Name -> Name -> Sem r ()
addName na nb = modify (HashMap.insert na nb)

foldApplication :: Expression -> [ApplicationArg] -> Expression
foldApplication f args = case args of
  [] -> f
  ((ApplicationArg i a) : as) -> foldApplication (ExpressionApplication (Application f a i)) as

matchFunctionParameter ::
  forall r.
  (Members '[State (HashMap Name Name), Reader (HashSet VarName), Error Text] r) =>
  FunctionParameter ->
  FunctionParameter ->
  Sem r ()
matchFunctionParameter pa pb = do
  goParamName (pa ^. paramName) (pb ^. paramName)
  goParamUsage (pa ^. paramUsage) (pb ^. paramUsage)
  goParamImplicit (pa ^. paramImplicit) (pb ^. paramImplicit)
  goParamType (pa ^. paramType) (pb ^. paramType)
  where
    goParamType :: Expression -> Expression -> Sem r ()
    goParamType ua ub = matchExpressions ua ub
    goParamImplicit :: IsImplicit -> IsImplicit -> Sem r ()
    goParamImplicit ua ub = unless (ua == ub) (throw @Text "implicit missmatch")
    goParamUsage :: Usage -> Usage -> Sem r ()
    goParamUsage ua ub = unless (ua == ub) (throw @Text "usage missmatch")
    goParamName :: Maybe VarName -> Maybe VarName -> Sem r ()
    goParamName (Just va) (Just vb) = addName va vb
    goParamName _ _ = return ()

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
          unlessM ((== Just vb) <$> gets @(HashMap Name Name) (^. at va)) err
        (_, _) -> unless (ia == ib) err
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
      (ExpressionUniverse ia, ExpressionUniverse ib) ->
        unless (ia == ib) err
      (ExpressionUniverse {}, _) -> err
      (_, ExpressionUniverse {}) -> err
      (ExpressionFunction ia, ExpressionFunction ib) ->
        goFunction ia ib
      (ExpressionFunction {}, _) -> err
      (_, ExpressionFunction {}) -> err
      (ExpressionLiteral ia, ExpressionLiteral ib) ->
        unless (ia == ib) err
      (ExpressionLiteral {}, _) -> err
      (_, ExpressionLiteral {}) -> err
      (ExpressionLet {}, ExpressionLet {}) -> error "not implemented"
      (_, ExpressionLet {}) -> err
      (ExpressionLet {}, _) -> err
      (ExpressionHole _, ExpressionHole _) -> return ()
    addIfFreeVar :: VarName -> VarName -> Sem r ()
    addIfFreeVar va vb = whenM (isSoftFreeVar va) (addName va vb)
    err :: Sem r a
    err = throw @Text "Expression missmatch"
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

class IsExpression a where
  toExpression :: a -> Expression

instance IsExpression ConstructorRef where
  toExpression = toExpression . IdenConstructor

instance IsExpression InductiveRef where
  toExpression = toExpression . IdenInductive

instance IsExpression FunctionRef where
  toExpression = toExpression . IdenFunction

instance IsExpression AxiomRef where
  toExpression = toExpression . IdenAxiom

instance IsExpression Iden where
  toExpression = ExpressionIden

instance IsExpression Expression where
  toExpression = id

instance IsExpression Name where
  toExpression n = case n ^. nameKind of
    KNameConstructor -> toExpression (ConstructorRef n)
    KNameInductive -> toExpression (InductiveRef n)
    KNameFunction -> toExpression (FunctionRef n)
    KNameAxiom -> toExpression (AxiomRef n)
    KNameLocal -> toExpression (IdenVar n)
    KNameLocalModule -> impossible
    KNameTopModule -> impossible

instance IsExpression Universe where
  toExpression = ExpressionUniverse

instance IsExpression Application where
  toExpression = ExpressionApplication

instance IsExpression Function where
  toExpression = ExpressionFunction

instance IsExpression ConstructorApp where
  toExpression (ConstructorApp c args) =
    foldApplication (toExpression c) (map toApplicationArg args)

isSmallUniverse' :: Expression -> Bool
isSmallUniverse' = \case
  ExpressionUniverse u -> isSmallUniverse u
  _ -> False

toApplicationArg :: PatternArg -> ApplicationArg
toApplicationArg p =
  set appArgIsImplicit (p ^. patternArgIsImplicit) (helper (p ^. patternArgPattern))
  where
    helper :: Pattern -> ApplicationArg
    helper = \case
      PatternVariable v -> ApplicationArg Explicit (toExpression v)
      PatternConstructorApp a -> ApplicationArg Explicit (toExpression a)
      PatternEmpty -> impossible
      PatternWildcard _ ->
        ApplicationArg
          Explicit
          ( ExpressionHole
              ( Hole
                  { _holeId = error "hole with no id",
                    _holeLoc = error "hole with no location"
                  }
              )
          )

clauseLhsAsExpression :: FunctionClause -> Expression
clauseLhsAsExpression cl =
  foldApplication (toExpression (cl ^. clauseName)) (map toApplicationArg (cl ^. clausePatterns))

expressionArrow :: (IsExpression a, IsExpression b) => IsImplicit -> a -> b -> Expression
expressionArrow isImplicit a b =
  ExpressionFunction
    ( Function
        ( FunctionParameter
            { _paramName = Nothing,
              _paramUsage = UsageOmega,
              _paramImplicit = isImplicit,
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

infix 4 ==%

(==%) :: (IsExpression a, IsExpression b) => a -> b -> HashSet Name -> Bool
(==%) a b free =
  isRight
    . run
    . runError @Text
    . runReader free
    . evalState (mempty @(HashMap Name Name))
    $ matchExpressions (toExpression a) (toExpression b)

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
        _nameLoc = error "freshVar with no location"
      }

freshHole :: (Member NameIdGen r) => Sem r Expression
freshHole = do
  uid <- freshNameId
  return $
    ExpressionHole
      ( Hole
          { _holeId = uid,
            _holeLoc = error "freshHole with no location"
          }
      )
