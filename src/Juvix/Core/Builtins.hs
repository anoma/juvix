module Juvix.Core.Builtins where

import Juvix.Prelude

-- Builtin operations which the evaluator and the code generator treat
-- specially and non-uniformly.
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
  | OpListHead
  | OpListTail
  | OpPairFst
  | OpPairSnd
  deriving stock (Eq)

-- Builtin data tags
data BuiltinDataTag
  = TagZero
  | TagSucc
  | TagUnit
  | TagNil
  | TagCons
  | TagPair
  deriving stock (Eq)
