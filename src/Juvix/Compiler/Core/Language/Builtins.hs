module Juvix.Compiler.Core.Language.Builtins where

import Juvix.Extra.Serialize
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
  | OpFieldAdd
  | OpFieldSub
  | OpFieldMul
  | OpFieldDiv
  | OpEq
  | OpShow
  | OpStrConcat
  | OpStrToInt
  | OpSeq
  | OpTrace
  | OpFail
  deriving stock (Eq, Generic)

instance Serialize BuiltinOp

-- Builtin data tags
data BuiltinDataTag
  = TagTrue
  | TagFalse
  | TagReturn
  | TagBind
  | TagWrite
  | TagReadLn
  deriving stock (Eq, Generic, Ord, Show)

instance Hashable BuiltinDataTag

instance Serialize BuiltinDataTag

builtinOpArgsNum :: BuiltinOp -> Int
builtinOpArgsNum = \case
  OpIntAdd -> 2
  OpIntSub -> 2
  OpIntMul -> 2
  OpIntDiv -> 2
  OpIntMod -> 2
  OpIntLt -> 2
  OpIntLe -> 2
  OpFieldAdd -> 2
  OpFieldSub -> 2
  OpFieldMul -> 2
  OpFieldDiv -> 2
  OpEq -> 2
  OpShow -> 1
  OpStrConcat -> 2
  OpStrToInt -> 1
  OpSeq -> 2
  OpTrace -> 1
  OpFail -> 1

builtinConstrArgsNum :: BuiltinDataTag -> Int
builtinConstrArgsNum = \case
  TagTrue -> 0
  TagFalse -> 0
  TagReturn -> 1
  TagBind -> 2
  TagWrite -> 1
  TagReadLn -> 0
