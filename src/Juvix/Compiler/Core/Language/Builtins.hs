module Juvix.Compiler.Core.Language.Builtins where

import GHC.Show qualified as Show
import Juvix.Extra.Serialize
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Prettyprinter

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
  | OpAssert
  | OpRangeCheck
  | OpTrace
  | OpFail
  | OpAnomaGet
  | OpAnomaEncode
  | OpAnomaDecode
  | OpAnomaVerifyDetached
  | OpAnomaSign
  | OpAnomaSignDetached
  | OpAnomaVerifyWithMessage
  | OpAnomaByteArrayToAnomaContents
  | OpAnomaByteArrayFromAnomaContents
  | OpAnomaSha256
  | OpAnomaResourceCommitment
  | OpAnomaResourceNullifier
  | OpAnomaResourceKind
  | OpAnomaResourceDelta
  | OpAnomaActionDelta
  | OpAnomaActionsDelta
  | OpAnomaProveAction
  | OpAnomaProveDelta
  | OpAnomaZeroDelta
  | OpAnomaAddDelta
  | OpAnomaSubDelta
  | OpAnomaRandomGeneratorInit
  | OpAnomaRandomNextBytes
  | OpAnomaRandomSplit
  | OpAnomaIsCommitment
  | OpAnomaIsNullifier
  | OpAnomaTransactionCompose
  | OpAnomaActionCreate
  | OpAnomaSetToList
  | OpAnomaSetFromList
  | OpPoseidonHash
  | OpEc
  | OpRandomEcPoint
  | OpUInt8ToInt
  | OpUInt8FromInt
  | OpByteArrayFromListByte
  | OpByteArrayLength
  deriving stock (Eq, Generic, Show)

instance Serialize BuiltinOp

instance NFData BuiltinOp

-- Builtin data tags
data BuiltinDataTag
  = TagTrue
  | TagFalse
  | TagReturn
  | TagBind
  | TagWrite
  | TagReadLn
  deriving stock (Eq, Generic, Ord)

instance Hashable BuiltinDataTag

instance Serialize BuiltinDataTag

instance NFData BuiltinDataTag

instance Pretty BuiltinDataTag where
  pretty = \case
    TagTrue -> Str.true_
    TagFalse -> Str.false_
    TagReturn -> Str.return
    TagBind -> Str.bind
    TagWrite -> Str.write
    TagReadLn -> Str.readLn

instance Show BuiltinDataTag where
  show = show . pretty

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
  OpAssert -> 1
  OpRangeCheck -> 2
  OpTrace -> 1
  OpFail -> 1
  OpAnomaGet -> 1
  OpAnomaEncode -> 1
  OpAnomaDecode -> 1
  OpAnomaVerifyDetached -> 3
  OpAnomaSign -> 2
  OpAnomaSignDetached -> 2
  OpAnomaVerifyWithMessage -> 2
  OpAnomaByteArrayToAnomaContents -> 1
  OpAnomaByteArrayFromAnomaContents -> 2
  OpAnomaSha256 -> 1
  OpAnomaResourceCommitment -> 1
  OpAnomaResourceNullifier -> 1
  OpAnomaResourceKind -> 1
  OpAnomaResourceDelta -> 1
  OpAnomaActionDelta -> 1
  OpAnomaActionsDelta -> 1
  OpAnomaProveAction -> 1
  OpAnomaProveDelta -> 1
  OpAnomaZeroDelta -> 0
  OpAnomaAddDelta -> 2
  OpAnomaSubDelta -> 2
  OpAnomaActionCreate -> 3
  OpAnomaTransactionCompose -> 2
  OpAnomaRandomGeneratorInit -> 1
  OpAnomaRandomNextBytes -> 2
  OpAnomaRandomSplit -> 1
  OpAnomaIsCommitment -> 1
  OpAnomaIsNullifier -> 1
  OpAnomaSetToList -> 1
  OpAnomaSetFromList -> 1
  OpPoseidonHash -> 1
  OpEc -> 3
  OpRandomEcPoint -> 0
  OpUInt8ToInt -> 1
  OpUInt8FromInt -> 1
  OpByteArrayFromListByte -> 1
  OpByteArrayLength -> 1

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
  OpAssert -> False
  OpTrace -> False
  OpFail -> False
  OpAnomaGet -> False
  OpAnomaEncode -> False
  OpAnomaDecode -> False
  OpAnomaVerifyDetached -> False
  OpAnomaSign -> False
  OpAnomaSignDetached -> False
  OpAnomaVerifyWithMessage -> False
  OpAnomaByteArrayToAnomaContents -> False
  OpAnomaByteArrayFromAnomaContents -> False
  OpAnomaResourceCommitment -> False
  OpAnomaResourceNullifier -> False
  OpAnomaResourceKind -> False
  OpAnomaResourceDelta -> False
  OpAnomaActionDelta -> False
  OpAnomaActionsDelta -> False
  OpAnomaProveAction -> False
  OpAnomaProveDelta -> False
  OpAnomaZeroDelta -> False
  OpAnomaAddDelta -> False
  OpAnomaSubDelta -> False
  OpAnomaSha256 -> False
  OpAnomaRandomGeneratorInit -> False
  OpAnomaRandomNextBytes -> False
  OpAnomaRandomSplit -> False
  OpAnomaIsCommitment -> False
  OpAnomaIsNullifier -> False
  OpAnomaTransactionCompose -> False
  OpAnomaActionCreate -> False
  OpAnomaSetToList -> False
  OpAnomaSetFromList -> False
  OpPoseidonHash -> False
  OpEc -> False
  OpRandomEcPoint -> False
  OpRangeCheck -> False
  OpUInt8ToInt -> True
  OpUInt8FromInt -> True
  OpByteArrayFromListByte -> False
  OpByteArrayLength -> False

builtinsString :: [BuiltinOp]
builtinsString = [OpStrConcat, OpStrToInt, OpShow]

builtinsCairo :: [BuiltinOp]
builtinsCairo = [OpPoseidonHash, OpEc, OpRandomEcPoint, OpRangeCheck]

builtinsAnoma :: [BuiltinOp]
builtinsAnoma =
  [ OpAnomaGet,
    OpAnomaEncode,
    OpAnomaDecode,
    OpAnomaVerifyDetached,
    OpAnomaSign,
    OpAnomaVerifyWithMessage,
    OpAnomaSignDetached,
    OpAnomaByteArrayToAnomaContents,
    OpAnomaByteArrayFromAnomaContents,
    OpAnomaSha256,
    OpAnomaResourceCommitment,
    OpAnomaResourceNullifier,
    OpAnomaResourceKind,
    OpAnomaResourceDelta,
    OpAnomaActionDelta,
    OpAnomaActionsDelta,
    OpAnomaProveAction,
    OpAnomaProveDelta,
    OpAnomaZeroDelta,
    OpAnomaAddDelta,
    OpAnomaSubDelta,
    OpAnomaRandomGeneratorInit,
    OpAnomaRandomNextBytes,
    OpAnomaRandomSplit,
    OpAnomaSetToList,
    OpAnomaSetFromList
  ]

builtinsUInt8 :: [BuiltinOp]
builtinsUInt8 = [OpUInt8FromInt, OpUInt8ToInt]

builtinsByteArray :: [BuiltinOp]
builtinsByteArray = [OpByteArrayFromListByte, OpByteArrayLength]
