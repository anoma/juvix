module Juvix.Compiler.Verification.Core.Extra where

import Juvix.Compiler.Verification.Core.Language
import Juvix.Prelude

mkVar :: Int -> Expr
mkVar = ExprVar . Var

mkCase :: Expr -> [(Name, Expr)] -> Maybe Expr -> Expr
mkCase scrutinee branches defaultBranch =
  ExprSave
    Save
      { _saveValue = scrutinee,
        _saveBody = go branches
      }
  where
    go :: [(Name, Expr)] -> Expr
    go = \case
      [] -> fromMaybe ExprFail defaultBranch
      ((name, branch) : rest) ->
        ExprBranch
          Branch
            { _branchConstr = name,
              _branchBody = branch,
              _branchNext = go rest
            }

mkInt :: Integer -> Expr
mkInt = ExprConst . ConstantInteger

mkApp :: Expr -> Expr -> Expr
mkApp left right = ExprApp App {_appLeft = left, _appRight = right}

mkConstrApp :: Expr -> Expr -> Expr
mkConstrApp left right = ExprConstrApp ConstrApp {_constrAppLeft = left, _constrAppRight = right}

mkConstrApps :: Name -> [Expr] -> Expr
mkConstrApps name args = foldl' mkConstrApp (ExprConstr name) args

mkLambda :: Expr -> Expr
mkLambda = ExprLambda . Lambda

mkSave :: Expr -> Expr -> Expr
mkSave value body = ExprSave Save {_saveValue = value, _saveBody = body}

mkBranch :: Name -> Expr -> Expr -> Expr
mkBranch name body next = ExprBranch Branch {_branchConstr = name, _branchBody = body, _branchNext = next}

mkRecur :: Expr -> Expr
mkRecur = ExprRecur . Recur
