module Juvix.Core.Language.Builtins where

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
  | OpIntLe
  | OpBoolAnd
  | OpBoolOr
  deriving stock (Eq)

-- Builtin data tags
data BuiltinDataTag
  = TagZero
  | TagSucc
  | TagUnit
  | TagNil
  | TagCons
  | TagPair
  deriving stock (Eq, Generic)

instance Hashable BuiltinDataTag

builtinOpArgsNum :: BuiltinOp -> Int
builtinOpArgsNum = \case
  OpIntAdd -> 2
  OpIntSub -> 2
  OpIntMul -> 2
  OpIntDiv -> 2
  OpIntEq -> 2
  OpIntLt -> 2
  OpIntLe -> 2
  OpBoolAnd -> 2
  OpBoolOr -> 2

builtinConstrArgsNum :: BuiltinDataTag -> Int
builtinConstrArgsNum = \case
  TagZero -> 0
  TagSucc -> 1
  TagUnit -> 0
  TagNil -> 0
  TagCons -> 2
  TagPair -> 2
