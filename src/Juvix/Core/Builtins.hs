module Juvix.Core.Builtins where

-- Builtin operations which the evaluator and the code generator treat
-- specially.
data Builtin
  = BuiltinIntAdd
  | BuiltinIntSub
  | BuiltinIntMul
  | BuiltinIntDiv
  | BuiltinIntEq
  | BuiltinIntLt
  | BuiltinIntGt
  | BuiltinIntLe
  | BuiltinIntGe
  | BuiltinBoolAnd
  | BuiltinBoolOr
