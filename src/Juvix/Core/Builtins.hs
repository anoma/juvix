module Juvix.Core.Builtins where

-- Builtin operations which the evaluator and the code generator treat
-- specially.
data BuiltinOp
  = OpIntAdd
  | OpIntSub
  | OpIntMul
  | OpIntDiv
  | OpIntEq
  | OpIntLt
  | OpIntGt
  | OpIntLe
  | OpIntGe
  | OpBoolAnd
  | OpBoolOr
