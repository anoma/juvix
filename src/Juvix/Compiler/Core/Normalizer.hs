module Juvix.Compiler.Core.Normalizer where

import Data.HashMap.Strict qualified as HashMap
import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Data.InfoTableBuilder
import Juvix.Compiler.Core.Evaluator
import Juvix.Compiler.Core.Extra.Base
import Juvix.Compiler.Core.Language

data NormEnv = NormEnv
  { -- | the symbols substituted for the free variables
    _normEnvVars :: HashMap Symbol Level,
    -- | current output de Bruijn level
    _normEnvLevel :: Level,
    -- | evaluation environment
    _normEnvEvalEnv :: Env
  }

makeLenses ''NormEnv

type Norm = Sem '[Reader NormEnv, InfoTableBuilder]

normalize :: InfoTable -> Node -> Node
normalize tab0 = run . evalInfoTableBuilder tab0 . runReader normEnv . normalize'
  where
    normEnv =
      NormEnv
        { _normEnvVars = mempty,
          _normEnvLevel = 0,
          _normEnvEvalEnv = []
        }

    normalize' :: Node -> Norm Node
    normalize' node0 = do
      val <- neval node0
      go val

    neval :: Node -> Norm Node
    neval node = do
      env <- asks (^. normEnvEvalEnv)
      tab <- getInfoTable
      return $ geval opts stdout (tab ^. identContext) env node
      where
        opts =
          EvalOptions
            { _evalOptionsNormalize = True
            }

    go :: Node -> Norm Node
    go val = case val of
      NVar x -> goVar x
      NIdt x -> goIdent x
      NCst {} -> return val
      NApp x -> goApp x
      NBlt x -> goBuiltinApp x
      NCtr x -> goConstr x
      NLam {} -> goLambdas val
      NLet x -> goLet x
      NRec {} -> impossible
      NCase x -> goCase x
      NMatch {} -> impossible
      NPi x -> goPi x
      NUniv {} -> return val
      NTyp x -> goTypeConstr x
      NPrim {} -> return val
      NDyn {} -> return val
      NBot {} -> return val
      Closure {..} -> goClosure _closureEnv _closureNode

    underBinder :: Norm a -> Norm a
    underBinder cont = do
      sym <- freshSymbol
      bl <- asks (^. normEnvLevel)
      local (over normEnvVars (HashMap.insert sym bl)) $
        local (over normEnvEvalEnv (mkIdent' sym :)) $
          local (over normEnvLevel (1 +)) cont
    {-# INLINE underBinder #-}

    underBinders :: Int -> Norm a -> Norm a
    underBinders 0 cont = cont
    underBinders k cont = underBinder (underBinders (k - 1) cont)

    goVar :: Var -> Norm Node
    goVar Var {..} = do
      env <- asks (^. normEnvEvalEnv)
      return $ env !! _varIndex
    {-# INLINE goVar #-}

    goIdent :: Ident -> Norm Node
    goIdent Ident {..} = do
      bl <- asks (^. normEnvLevel)
      vars <- asks (^. normEnvVars)
      return $
        mkVar' $
          getBinderIndex bl $
            fromJust $
              HashMap.lookup _identSymbol vars
    {-# INLINE goIdent #-}

    goApp :: App -> Norm Node
    goApp App {..} = do
      l <- go _appLeft
      r <- go _appRight
      return $ mkApp _appInfo l r
    {-# INLINE goApp #-}

    goBuiltinApp :: BuiltinApp -> Norm Node
    goBuiltinApp BuiltinApp {..} = do
      args <- mapM go _builtinAppArgs
      return $ mkBuiltinApp _builtinAppInfo _builtinAppOp args
    {-# INLINE goBuiltinApp #-}

    goConstr :: Constr -> Norm Node
    goConstr Constr {..} = do
      args <- mapM go _constrArgs
      return $ mkConstr _constrInfo _constrTag args
    {-# INLINE goConstr #-}

    goLambdas :: Node -> Norm Node
    goLambdas = \case
      NLam Lambda {..} -> do
        type' <- go (_lambdaBinder ^. binderType)
        let binder' = set binderType type' _lambdaBinder
        body' <- underBinder (goLambdas _lambdaBody)
        return $ mkLambda _lambdaInfo binder' body'
      node ->
        normalize' node

    goLet :: Let -> Norm Node
    goLet Let {..} = do
      type' <- go (_letItem ^. letItemBinder . binderType)
      let binder' = set binderType type' (_letItem ^. letItemBinder)
      value' <- go (_letItem ^. letItemValue)
      body' <- underBinder (normalize' _letBody)
      return $ mkLet _letInfo binder' value' body'
    {-# INLINE goLet #-}

    goCase :: Case -> Norm Node
    goCase Case {..} = do
      value' <- go _caseValue
      brs' <- mapM goBranch _caseBranches
      def' <-
        case _caseDefault of
          Just def -> Just <$> normalize' def
          Nothing -> return Nothing
      return $ mkCase _caseInfo _caseInductive value' brs' def'
    {-# INLINE goCase #-}

    goBranch :: CaseBranch -> Norm CaseBranch
    goBranch CaseBranch {..} = do
      binders' <- goBinders _caseBranchBinders
      body' <- underBinders _caseBranchBindersNum (normalize' _caseBranchBody)
      return $ CaseBranch _caseBranchInfo _caseBranchTag binders' _caseBranchBindersNum body'

    goBinders :: [Binder] -> Norm [Binder]
    goBinders = \case
      [] -> return []
      h : t -> do
        type' <- normalize' (h ^. binderType)
        t' <- underBinder (goBinders t)
        return $ set binderType type' h : t'

    goPi :: Pi -> Norm Node
    goPi Pi {..} = do
      type' <- go (_piBinder ^. binderType)
      let binder' = set binderType type' _piBinder
      body' <- underBinder (normalize' _piBody)
      return $ mkPi _piInfo binder' body'
    {-# INLINE goPi #-}

    goTypeConstr :: TypeConstr -> Norm Node
    goTypeConstr TypeConstr {..} = do
      args' <- mapM go _typeConstrArgs
      return $ mkTypeConstr _typeConstrInfo _typeConstrSymbol args'
    {-# INLINE goTypeConstr #-}

    goClosure :: Env -> Node -> Norm Node
    goClosure env node = local (set normEnvEvalEnv env) (go node)
