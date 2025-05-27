module Juvix.Compiler.Internal.Extra.Clonable
  ( Clonable,
    clone,
    clones,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Internal.Extra.Base
import Juvix.Compiler.Internal.Extra.Binders
import Juvix.Compiler.Internal.Language
import Juvix.Prelude

type FreshBindersContext = HashMap NameId NameId

clones :: (Clonable a, Members '[NameIdGen] r, Traversable l) => l a -> Sem r (l a)
clones = runReader iniCtx . mapM freshNameIds
  where
    iniCtx :: FreshBindersContext
    iniCtx = mempty

clone :: (Clonable a, Members '[NameIdGen] r) => a -> Sem r a
clone = runReader iniCtx . freshNameIds
  where
    iniCtx :: FreshBindersContext
    iniCtx = mempty

class Clonable a where
  freshNameIds :: (Members '[Reader FreshBindersContext, NameIdGen] r) => a -> Sem r a

instance Clonable InductiveParameter where
  freshNameIds InductiveParameter {..} = do
    ty' <- freshNameIds _inductiveParamType
    return
      InductiveParameter
        { _inductiveParamName,
          _inductiveParamType = ty'
        }

instance Clonable Name where
  freshNameIds n = do
    ctx <- ask @FreshBindersContext
    return $ case ctx ^. at (n ^. nameId) of
      Nothing -> n
      Just uid' -> set nameId uid' n

instance Clonable Iden where
  freshNameIds = traverseOf idenName freshNameIds

instance Clonable Application where
  freshNameIds Application {..} = do
    l' <- freshNameIds _appLeft
    r' <- freshNameIds _appRight
    return
      Application
        { _appLeft = l',
          _appRight = r',
          _appImplicit
        }

instance (Clonable a) => Clonable (WithLoc a) where
  freshNameIds = traverseOf withLocParam freshNameIds

instance Clonable Literal where
  freshNameIds = return

instance Clonable InstanceHole where
  freshNameIds = return

instance Clonable Hole where
  freshNameIds = return

instance Clonable SmallUniverse where
  freshNameIds = return

instance (Clonable a) => Clonable [a] where
  freshNameIds = mapM freshNameIds

instance (Clonable a) => Clonable (Maybe a) where
  freshNameIds = mapM freshNameIds

underBinder ::
  forall r a binding.
  (HasBinders binding, Members '[Reader FreshBindersContext, NameIdGen] r) =>
  binding ->
  (binding -> Sem r a) ->
  Sem r a
underBinder p f = underBinders [p] (f . headDef impossible)

underBindersNonEmpty ::
  forall r a binding.
  (HasBinders binding, Members '[Reader FreshBindersContext, NameIdGen] r) =>
  NonEmpty binding ->
  (NonEmpty binding -> Sem r a) ->
  Sem r a
underBindersNonEmpty p f = underBinders (toList p) (f . nonEmpty')

underClonableBindersNonEmpty :: forall r a binding. (Clonable binding, HasBinders binding, Members '[Reader FreshBindersContext, NameIdGen] r) => NonEmpty binding -> (NonEmpty binding -> Sem r a) -> Sem r a
underClonableBindersNonEmpty ps0 f = underClonableBinders (toList ps0) (f . nonEmpty')

underClonableBinders :: forall r a binding. (Clonable binding, HasBinders binding, Members '[Reader FreshBindersContext, NameIdGen] r) => [binding] -> ([binding] -> Sem r a) -> Sem r a
underClonableBinders binders f = do
  ctx <- ask @FreshBindersContext
  let bindersIds :: [NameId] = binders ^.. each . bindersTraversal . nameId
  bindersIds' <- mapM (const freshNameId) bindersIds
  let ctx' = ctx <> HashMap.fromList (zipExact bindersIds bindersIds')
  local (const ctx') $ do
    binders' <- freshNameIds binders
    f binders'

underBinders :: forall r a binding. (HasBinders binding, Members '[Reader FreshBindersContext, NameIdGen] r) => [binding] -> ([binding] -> Sem r a) -> Sem r a
underBinders ps f = do
  ctx <- ask @FreshBindersContext
  (ctx', ps') <- runState ctx (mapM goBinders ps)
  local (const ctx') (f ps')
  where
    goBinders :: forall r'. (Members '[State FreshBindersContext, NameIdGen] r') => binding -> Sem r' binding
    goBinders pat = forOf bindersTraversal pat addVar
      where
        addVar :: VarName -> Sem r' VarName
        addVar v = do
          ctx <- get @FreshBindersContext
          uid' <- maybe freshNameId return (ctx ^. at (v ^. nameId))
          modify' @FreshBindersContext (set (at (v ^. nameId)) (Just uid'))
          return (set nameId uid' v)

instance Clonable SideIfBranch where
  freshNameIds = directExpressions freshNameIds

instance Clonable SideIfs where
  freshNameIds = directExpressions freshNameIds

instance Clonable CaseBranchRhs where
  freshNameIds = \case
    CaseBranchRhsExpression e -> CaseBranchRhsExpression <$> freshNameIds e
    CaseBranchRhsIf e -> CaseBranchRhsIf <$> freshNameIds e

instance Clonable CaseBranch where
  freshNameIds CaseBranch {..} =
    underBinder _caseBranchPattern $ \pat' -> do
      body' <- freshNameIds _caseBranchRhs
      return
        CaseBranch
          { _caseBranchPattern = pat',
            _caseBranchRhs = body'
          }

instance Clonable Case where
  freshNameIds Case {..} = do
    e' <- freshNameIds _caseExpression
    ety' <- freshNameIds _caseExpressionType
    wholetype' <- freshNameIds _caseExpressionWholeType
    branches' <- mapM freshNameIds _caseBranches
    return
      Case
        { _caseExpression = e',
          _caseExpressionType = ety',
          _caseExpressionWholeType = wholetype',
          _caseBranches = branches'
        }

instance Clonable Function where
  freshNameIds Function {..} = do
    ty' <- freshNameIds (_functionLeft ^. paramType)
    underBinder _functionLeft $ \l' -> do
      r' <- freshNameIds _functionRight
      return
        Function
          { _functionLeft = set paramType ty' l',
            _functionRight = r'
          }

instance (Clonable a) => Clonable (NonEmpty a) where
  freshNameIds = mapM freshNameIds

instance Clonable MutualBlockLet where
  freshNameIds MutualBlockLet {..} =
    underClonableBindersNonEmpty _mutualLet $ \funs -> do
      return
        MutualBlockLet
          { _mutualLet = funs
          }

instance Clonable LetClause where
  freshNameIds = \case
    LetFunDef f -> LetFunDef <$> freshNameIds f
    LetMutualBlock m -> LetMutualBlock <$> freshNameIds m

instance Clonable Let where
  freshNameIds :: (Members '[Reader FreshBindersContext, NameIdGen] r) => Let -> Sem r Let
  freshNameIds Let {..} = do
    underClonableBindersNonEmpty _letClauses $ \clauses' -> do
      e' <- freshNameIds _letExpression
      return
        Let
          { _letClauses = clauses',
            _letExpression = e'
          }

instance Clonable SimpleBinder where
  freshNameIds SimpleBinder {..} = do
    ty' <- freshNameIds _sbinderType
    return
      SimpleBinder
        { _sbinderType = ty',
          _sbinderVar
        }

instance Clonable SimpleLambda where
  freshNameIds SimpleLambda {..} =
    underBinder _slambdaBinder $ \bi -> do
      bi' <- freshNameIds bi
      body' <- freshNameIds _slambdaBody
      return
        SimpleLambda
          { _slambdaBinder = bi',
            _slambdaBody = body'
          }

instance Clonable LambdaClause where
  freshNameIds LambdaClause {..} =
    underBindersNonEmpty _lambdaPatterns $ \ps' -> do
      body' <- freshNameIds _lambdaBody
      return
        LambdaClause
          { _lambdaPatterns = ps',
            _lambdaBody = body'
          }

instance Clonable Lambda where
  freshNameIds Lambda {..} = do
    ty' <- freshNameIds _lambdaType
    clauses' <- freshNameIds _lambdaClauses
    return
      Lambda
        { _lambdaType = ty',
          _lambdaClauses = clauses'
        }

instance Clonable BuiltinNatural where
  freshNameIds = traverseOf builtinNaturalArg freshNameIds

instance Clonable Expression where
  freshNameIds :: (Members '[Reader FreshBindersContext, NameIdGen] r) => Expression -> Sem r Expression
  freshNameIds = \case
    ExpressionIden i -> ExpressionIden <$> freshNameIds i
    ExpressionNatural i -> ExpressionNatural <$> freshNameIds i
    ExpressionApplication a -> ExpressionApplication <$> freshNameIds a
    ExpressionLiteral a -> ExpressionLiteral <$> freshNameIds a
    ExpressionHole a -> ExpressionHole <$> freshNameIds a
    ExpressionUniverse a -> ExpressionUniverse <$> freshNameIds a
    ExpressionCase a -> ExpressionCase <$> freshNameIds a
    ExpressionFunction f -> ExpressionFunction <$> freshNameIds f
    ExpressionInstanceHole h -> ExpressionInstanceHole <$> freshNameIds h
    ExpressionLet l -> ExpressionLet <$> freshNameIds l
    ExpressionSimpleLambda l -> ExpressionSimpleLambda <$> freshNameIds l
    ExpressionLambda l -> ExpressionLambda <$> freshNameIds l

instance Clonable ArgInfo where
  freshNameIds ArgInfo {..} = do
    sig' <- mapM freshNameIds _argInfoDefault
    return
      ArgInfo
        { _argInfoDefault = sig',
          _argInfoName
        }

-- | Note that the name of the function is fresh. This is desirable when the
-- functionDef is part of a let.
instance Clonable FunctionDef where
  freshNameIds :: (Members '[Reader FreshBindersContext, NameIdGen] r) => FunctionDef -> Sem r FunctionDef
  freshNameIds fun@FunctionDef {..} = do
    ty' <- freshNameIds _funDefType
    underBinder fun $ \fun' -> do
      body' <- freshNameIds _funDefBody
      defaultSig' <- freshNameIds _funDefArgsInfo
      return
        FunctionDef
          { _funDefName = fun' ^. funDefName,
            _funDefType = ty',
            _funDefBody = body',
            _funDefArgsInfo = defaultSig',
            _funDefTerminating,
            _funDefIsInstanceCoercion,
            _funDefBuiltin,
            _funDefPragmas,
            _funDefDocComment
          }
