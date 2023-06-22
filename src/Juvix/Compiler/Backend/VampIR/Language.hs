module Juvix.Compiler.Backend.VampIR.Language
  ( module Juvix.Compiler.Backend.VampIR.Language,
    module Juvix.Prelude,
  )
where

import Juvix.Prelude

newtype Var = Var
  { _varName :: Text
  }

data OpCode
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpLt
  | OpLe

data Binop = Binop
  { _binopOp :: OpCode,
    _binopLeft :: Expression,
    _binopRight :: Expression
  }

data IfThenElse = IfThenElse
  { _ifThenElseCondition :: Expression,
    _ifThenElseBranchTrue :: Expression,
    _ifThenElseBranchFalse :: Expression
  }

data Expression
  = ExpressionVar Var
  | ExpressionConstant Integer
  | ExpressionBinop Binop
  | ExpressionIfThenElse IfThenElse
  | ExpressionFail

data LocalDef = LocalDef
  { _localDefName :: Text,
    _localDefValue :: Expression
  }

data Function = Function
  { _functionName :: Text,
    _functionArguments :: [Text],
    _functionLocalDefs :: [LocalDef],
    _functionExpression :: Expression,
    _functionInputs :: [Text],
    _functionOutput :: Text
  }

data Program = Program
  { _programFunctions :: [Function],
    _programPublicInputs :: [Text]
  }

makeLenses ''Var
makeLenses ''Binop
makeLenses ''IfThenElse
makeLenses ''Expression
makeLenses ''LocalDef
makeLenses ''Function
makeLenses ''Program

instance HasAtomicity Var where
  atomicity _ = Atom

instance HasAtomicity Binop where
  atomicity _ = Aggregate appFixity

instance HasAtomicity IfThenElse where
  atomicity _ = Aggregate appFixity

instance HasAtomicity Expression where
  atomicity = \case
    ExpressionVar x -> atomicity x
    ExpressionConstant {} -> Atom
    ExpressionBinop x -> atomicity x
    ExpressionIfThenElse x -> atomicity x
    ExpressionFail -> Atom
