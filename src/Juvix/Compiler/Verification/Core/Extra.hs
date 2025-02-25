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
