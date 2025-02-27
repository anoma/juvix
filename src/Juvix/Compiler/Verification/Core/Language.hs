module Juvix.Compiler.Verification.Core.Language where

import Juvix.Prelude

data Constant = ConstantInteger Integer

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
makeLenses ''Recur

instance HasAtomicity Var where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Constant where
  atomicity _ = Aggregate appFixity

instance HasAtomicity App where
  atomicity _ = Aggregate appFixity

instance HasAtomicity ConstrApp where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Binop where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Lambda where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Save where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Branch where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Recur where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Expr where
  atomicity = \case
    ExprVar v -> atomicity v
    ExprUnit -> Atom
    ExprConst c -> atomicity c
    ExprConstr _ -> Aggregate appFixity
    ExprApp a -> atomicity a
    ExprConstrApp c -> atomicity c
    ExprBinop b -> atomicity b
    ExprLambda l -> atomicity l
    ExprSave s -> atomicity s
    ExprBranch b -> atomicity b
    ExprRecur r -> atomicity r
    ExprFail -> Atom
