module Juvix.Compiler.Core.Language.Builtins where

import GHC.Show qualified as Show
import Juvix.Extra.Serialize
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Prettyprinter

data BuiltinCategory
  = BuiltinCategoryString
  | BuiltinCategoryCairo
  | BuiltinCategoryAnoma
  | BuiltinCategoryUInt8
  | BuiltinCategoryByteArray
  | BuiltinCategoryNone
  deriving stock (Eq, Enum, Ord, Bounded, Show, Data, Generic)

builtinCategory :: BuiltinOp -> BuiltinCategory
builtinCategory = \case
  OpIntAdd -> BuiltinCategoryNone
  OpIntSub -> BuiltinCategoryNone
  OpIntMul -> BuiltinCategoryNone
  OpIntDiv -> BuiltinCategoryNone
  OpIntMod -> BuiltinCategoryNone
  OpIntLt -> BuiltinCategoryNone
  OpIntLe -> BuiltinCategoryNone
  OpFieldAdd -> BuiltinCategoryNone
  OpFieldSub -> BuiltinCategoryNone
  OpFieldMul -> BuiltinCategoryNone
  OpFieldDiv -> BuiltinCategoryNone
  OpFieldFromInt -> BuiltinCategoryNone
  OpFieldToInt -> BuiltinCategoryNone
  OpEq -> BuiltinCategoryNone
  OpShow -> BuiltinCategoryString
  OpStrConcat -> BuiltinCategoryString
  OpStrToInt -> BuiltinCategoryString
  OpSeq -> BuiltinCategoryNone
  OpAssert -> BuiltinCategoryNone
  OpTrace -> BuiltinCategoryNone
  OpFail -> BuiltinCategoryNone
  OpAnomaGet -> BuiltinCategoryAnoma
  OpAnomaEncode -> BuiltinCategoryAnoma
  OpAnomaDecode -> BuiltinCategoryAnoma
  OpAnomaVerifyDetached -> BuiltinCategoryAnoma
  OpAnomaSign -> BuiltinCategoryAnoma
  OpAnomaSignDetached -> BuiltinCategoryAnoma
  OpAnomaVerifyWithMessage -> BuiltinCategoryAnoma
  OpAnomaByteArrayToAnomaContents -> BuiltinCategoryAnoma
  OpAnomaByteArrayFromAnomaContents -> BuiltinCategoryAnoma
  OpAnomaSha256 -> BuiltinCategoryAnoma
  OpAnomaResourceCommitment -> BuiltinCategoryAnoma
  OpAnomaResourceNullifier -> BuiltinCategoryAnoma
  OpAnomaResourceKind -> BuiltinCategoryAnoma
  OpAnomaResourceDelta -> BuiltinCategoryAnoma
  OpAnomaActionDelta -> BuiltinCategoryAnoma
  OpAnomaActionsDelta -> BuiltinCategoryAnoma
  OpAnomaZeroDelta -> BuiltinCategoryAnoma
  OpAnomaAddDelta -> BuiltinCategoryAnoma
  OpAnomaSubDelta -> BuiltinCategoryAnoma
  OpAnomaRandomGeneratorInit -> BuiltinCategoryAnoma
  OpAnomaRandomNextBytes -> BuiltinCategoryAnoma
  OpAnomaRandomSplit -> BuiltinCategoryAnoma
  OpAnomaIsCommitment -> BuiltinCategoryAnoma
  OpAnomaIsNullifier -> BuiltinCategoryAnoma
  OpAnomaCreateFromComplianceInputs -> BuiltinCategoryAnoma
  OpAnomaTransactionCompose -> BuiltinCategoryAnoma
  OpAnomaActionCreate -> BuiltinCategoryAnoma
  OpAnomaSetToList -> BuiltinCategoryAnoma
  OpAnomaSetFromList -> BuiltinCategoryAnoma
  OpNockmaReify -> BuiltinCategoryAnoma
  OpPoseidonHash -> BuiltinCategoryCairo
  OpEc -> BuiltinCategoryCairo
  OpRandomEcPoint -> BuiltinCategoryCairo
  OpRangeCheck -> BuiltinCategoryCairo
  OpUInt8ToInt -> BuiltinCategoryUInt8
  OpUInt8FromInt -> BuiltinCategoryUInt8
  OpByteArrayFromListByte -> BuiltinCategoryByteArray
  OpByteArrayLength -> BuiltinCategoryByteArray

-- | Builtin operations which the evaluator and the code generator treat
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
  | OpAnomaZeroDelta
  | OpAnomaAddDelta
  | OpAnomaSubDelta
  | OpAnomaRandomGeneratorInit
  | OpAnomaRandomNextBytes
  | OpAnomaRandomSplit
  | OpAnomaIsCommitment
  | OpAnomaIsNullifier
  | OpAnomaCreateFromComplianceInputs
  | OpAnomaTransactionCompose
  | OpAnomaActionCreate
  | OpAnomaSetToList
  | OpAnomaSetFromList
  | OpNockmaReify
  | OpPoseidonHash
  | OpEc
  | OpRandomEcPoint
  | OpUInt8ToInt
  | OpUInt8FromInt
  | OpByteArrayFromListByte
  | OpByteArrayLength
  deriving stock (Eq, Generic, Show, Enum, Bounded)

instance Serialize BuiltinOp

instance NFData BuiltinOp

-- | Builtin data tags
data BuiltinDataTag
  = TagTrue
  | TagFalse
  | TagReturn
  | TagBind
  | TagWrite
  | TagReadLn
  | TagJsonArray
  | TagJsonBool
  | TagJsonObject
  | TagJsonNumber
  | TagJsonString
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
    TagJsonArray -> Str.jsonArray
    TagJsonBool -> Str.jsonBool
    TagJsonObject -> Str.jsonObject
    TagJsonNumber -> Str.jsonNumber
    TagJsonString -> Str.jsonString

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
  OpAnomaCreateFromComplianceInputs -> 5
  OpAnomaSetToList -> 1
  OpAnomaSetFromList -> 1
  OpPoseidonHash -> 1
  OpEc -> 3
  OpRandomEcPoint -> 0
  OpUInt8ToInt -> 1
  OpUInt8FromInt -> 1
  OpByteArrayFromListByte -> 1
  OpByteArrayLength -> 1
  OpNockmaReify -> 1

builtinConstrArgsNum :: BuiltinDataTag -> Int
builtinConstrArgsNum = \case
  TagTrue -> 0
  TagFalse -> 0
  TagReturn -> 1
  TagBind -> 2
  TagWrite -> 1
  TagReadLn -> 0
  TagJsonArray -> 1
  TagJsonBool -> 1
  TagJsonObject -> 1
  TagJsonNumber -> 1
  TagJsonString -> 1

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
  OpAnomaZeroDelta -> False
  OpAnomaAddDelta -> False
  OpAnomaSubDelta -> False
  OpAnomaSha256 -> False
  OpAnomaRandomGeneratorInit -> False
  OpAnomaRandomNextBytes -> False
  OpAnomaRandomSplit -> False
  OpAnomaIsCommitment -> False
  OpAnomaIsNullifier -> False
  OpAnomaCreateFromComplianceInputs -> False
  OpAnomaTransactionCompose -> False
  OpAnomaActionCreate -> False
  OpAnomaSetToList -> False
  OpAnomaSetFromList -> False
  OpNockmaReify -> False
  OpPoseidonHash -> False
  OpEc -> False
  OpRandomEcPoint -> False
  OpRangeCheck -> False
  OpUInt8ToInt -> True
  OpUInt8FromInt -> True
  OpByteArrayFromListByte -> False
  OpByteArrayLength -> False

builtinsInCategory :: BuiltinCategory -> [BuiltinOp]
builtinsInCategory c = filter ((== c) . builtinCategory) allElements

builtinsString :: [BuiltinOp]
builtinsString = builtinsInCategory BuiltinCategoryString

builtinsCairo :: [BuiltinOp]
builtinsCairo = builtinsInCategory BuiltinCategoryCairo

builtinsAnoma :: [BuiltinOp]
builtinsAnoma = builtinsInCategory BuiltinCategoryAnoma

builtinsUInt8 :: [BuiltinOp]
builtinsUInt8 = builtinsInCategory BuiltinCategoryUInt8

builtinsByteArray :: [BuiltinOp]
builtinsByteArray = builtinsInCategory BuiltinCategoryByteArray
