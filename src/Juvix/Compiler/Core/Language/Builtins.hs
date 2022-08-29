module Juvix.Compiler.Core.Language.Builtins where

import Juvix.Prelude

-- Builtin operations which the evaluator and the code generator treat
-- specially and non-uniformly.
data BuiltinOp
  = OpIntAdd
  | OpIntSub
  | OpIntMul
  | OpIntDiv
  | OpIntMod
  | OpIntLt
  | OpIntLe
  | OpEq
  | OpTrace
  | OpFail
  deriving stock (Eq)

-- Builtin data tags
data BuiltinDataTag
  = TagVoid
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
  OpIntMod -> 2
  OpIntLt -> 2
  OpIntLe -> 2
  OpEq -> 2
  OpTrace -> 2
  OpFail -> 1

builtinConstrArgsNum :: BuiltinDataTag -> Int
builtinConstrArgsNum = \case
  TagVoid -> 0
  TagReturn -> 1
  TagBind -> 2
  TagWrite -> 1
  TagReadLn -> 0
