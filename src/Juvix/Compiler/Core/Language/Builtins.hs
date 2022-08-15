module Juvix.Compiler.Core.Language.Builtins where

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
  deriving stock (Eq)

-- Builtin data tags
data BuiltinDataTag
  = TagNil
  | TagCons
  | TagReturn
  | TagBind
  | TagWrite
  | TagReadLn
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

builtinConstrArgsNum :: BuiltinDataTag -> Int
builtinConstrArgsNum = \case
  TagNil -> 0
  TagCons -> 2
  TagReturn -> 1
  TagBind -> 2
  TagWrite -> 1
  TagReadLn -> 0
