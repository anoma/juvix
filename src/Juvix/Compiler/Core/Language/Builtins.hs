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
  | OpFieldFromInt
  | OpFieldToInt
  | OpEq
  | OpShow
  | OpStrConcat
  | OpStrToInt
  | OpSeq
  | OpTrace
  | OpFail
  | OpAnomaGet
  | OpPoseidonHash
  | OpEc
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
  OpFieldFromInt -> 1
  OpFieldToInt -> 1
  OpEq -> 2
  OpShow -> 1
  OpStrConcat -> 2
  OpStrToInt -> 1
  OpSeq -> 2
  OpTrace -> 1
  OpFail -> 1
  OpAnomaGet -> 1
  OpPoseidonHash -> 1
  OpEc -> 3

builtinConstrArgsNum :: BuiltinDataTag -> Int
builtinConstrArgsNum = \case
  TagTrue -> 0
  TagFalse -> 0
  TagReturn -> 1
  TagBind -> 2
  TagWrite -> 1
  TagReadLn -> 0

-- | True if the builtin can be evaluated away during constant folding
builtinIsFoldable :: BuiltinOp -> Bool
builtinIsFoldable = \case
  OpIntAdd -> True
  OpIntSub -> True
  OpIntMul -> True
  OpIntDiv -> True
  OpIntMod -> True
  OpIntLt -> True
  OpIntLe -> True
  OpFieldAdd -> True
  OpFieldSub -> True
  OpFieldMul -> True
  OpFieldDiv -> True
  OpFieldFromInt -> True
  OpFieldToInt -> True
  OpEq -> True
  OpShow -> True
  OpStrConcat -> True
  OpStrToInt -> True
  OpSeq -> False
  OpTrace -> False
  OpFail -> False
  OpAnomaGet -> False
  OpPoseidonHash -> False
  OpEc -> False

builtinIsCairo :: BuiltinOp -> Bool
builtinIsCairo = \case
  OpPoseidonHash -> True
  OpEc -> True
  _ -> False
