module Juvix.Compiler.Verification.Core.Language where

import Juvix.Prelude

data Constant = ConstantInt Int

type Name = Text

data BinaryOp
  = BinaryOpAdd
  | BinaryOpSub
  | BinaryOpMul
  | BinaryOpDiv

data Expr
  = ExprVar Var
  | ExprUnit
  | ExprConst Constant
  | ExprConstr Name
  | ExprApp App
  | ExprConstrApp ConstrApp
  | ExprBinop Binop
  | ExprLambda Lambda
  | ExprSave Save
  | ExprBranch Branch
  | ExprDefault Default
  | ExprRecur Recur
  | ExprFail

data Var = Var
  { _varIndex :: Int
  }

data App = App
  { _appLeft :: Expr,
    _appRight :: Expr
  }

data ConstrApp = ConstrApp
  { _constrAppLeft :: Expr,
    _constrAppRight :: Expr
  }

data Binop = Binop
  { _binopOper :: BinaryOp,
    _binopArg1 :: Expr,
    _binopArg2 :: Expr
  }

data Lambda = Lambda
  { _lambdaBody :: Expr
  }

data Save = Save
  { _saveValue :: Expr,
    _saveBody :: Expr
  }

data Branch = Branch
  { _branchConstr :: Name,
    _branchBody :: Expr,
    _branchNext :: Expr
  }

data Default = Default
  { _defaultBody :: Expr
  }

data Recur = Recur
  { _recurBody :: Expr
  }

makeLenses ''Var
makeLenses ''App
makeLenses ''ConstrApp
makeLenses ''Binop
makeLenses ''Lambda
makeLenses ''Save
makeLenses ''Branch
makeLenses ''Default
makeLenses ''Recur
