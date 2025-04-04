module Juvix.Compiler.Concrete.Data.Builtins where

import Data.Serialize
import Juvix.Data.NameKind
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

class IsBuiltin a where
  toBuiltinPrim :: a -> BuiltinPrim

instance (IsBuiltin a) => IsBuiltin (WithLoc a) where
  toBuiltinPrim = toBuiltinPrim . (^. withLocParam)

instance IsBuiltin BuiltinPrim where
  toBuiltinPrim = id

instance IsBuiltin BuiltinInductive where
  toBuiltinPrim = BuiltinsInductive

instance IsBuiltin BuiltinConstructor where
  toBuiltinPrim = BuiltinsConstructor

instance IsBuiltin BuiltinFunction where
  toBuiltinPrim = BuiltinsFunction

instance IsBuiltin BuiltinAxiom where
  toBuiltinPrim = BuiltinsAxiom

data BuiltinPrim
  = BuiltinsInductive BuiltinInductive
  | BuiltinsConstructor BuiltinConstructor
  | BuiltinsFunction BuiltinFunction
  | BuiltinsAxiom BuiltinAxiom
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinPrim

instance Serialize BuiltinPrim

instance NFData BuiltinPrim

instance Pretty BuiltinPrim where
  pretty = \case
    BuiltinsInductive i -> pretty i
    BuiltinsConstructor c -> pretty c
    BuiltinsFunction f -> pretty f
    BuiltinsAxiom a -> pretty a

builtinConstructors :: BuiltinInductive -> [BuiltinConstructor]
builtinConstructors = \case
  BuiltinNat -> [BuiltinNatZero, BuiltinNatSuc]
  BuiltinBool -> [BuiltinBoolFalse, BuiltinBoolTrue]
  BuiltinInt -> [BuiltinIntOfNat, BuiltinIntNegSuc]
  BuiltinList -> [BuiltinListNil, BuiltinListCons]
  BuiltinMaybe -> [BuiltinMaybeNothing, BuiltinMaybeJust]
  BuiltinPair -> [BuiltinPairConstr]
  BuiltinJson -> [BuiltinJsonArray, BuiltinJsonBool, BuiltinJsonObject, BuiltinJsonNumber, BuiltinJsonString]
  BuiltinPoseidonState -> [BuiltinMkPoseidonState]
  BuiltinEcPoint -> [BuiltinMkEcPoint]
  BuiltinAnomaResource -> [BuiltinMkAnomaResource]
  BuiltinAnomaAction -> [BuiltinMkAnomaAction]
  BuiltinAnomaComplianceInputs -> [BuiltinMkAnomaComplianceInputs]
  BuiltinAnomaShieldedTransaction -> [BuiltinMkAnomaShieldedTransaction]
  BuiltinEq -> [BuiltinMkEq]
  BuiltinOrd -> [BuiltinMkOrd]
  BuiltinOrdering -> [BuiltinOrderingLT, BuiltinOrderingEQ, BuiltinOrderingGT]
  BuiltinNockmaNoun -> [BuiltinNockmaAtom, BuiltinNockmaCell]

data BuiltinInductive
  = BuiltinNat
  | BuiltinBool
  | BuiltinInt
  | BuiltinList
  | BuiltinMaybe
  | BuiltinPair
  | BuiltinJson
  | BuiltinEq
  | BuiltinOrd
  | BuiltinOrdering
  | BuiltinPoseidonState
  | BuiltinEcPoint
  | BuiltinAnomaResource
  | BuiltinAnomaAction
  | BuiltinAnomaComplianceInputs
  | BuiltinAnomaShieldedTransaction
  | BuiltinNockmaNoun
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinInductive

instance Serialize BuiltinInductive

instance NFData BuiltinInductive

instance Pretty BuiltinInductive where
  pretty = \case
    BuiltinNat -> Str.nat
    BuiltinBool -> Str.bool_
    BuiltinInt -> Str.int_
    BuiltinList -> Str.list
    BuiltinMaybe -> Str.maybe_
    BuiltinPair -> Str.pair
    BuiltinJson -> Str.json
    BuiltinEq -> Str.eq
    BuiltinOrd -> Str.ord
    BuiltinOrdering -> Str.ordering
    BuiltinPoseidonState -> Str.cairoPoseidonState
    BuiltinEcPoint -> Str.cairoEcPoint
    BuiltinAnomaResource -> Str.anomaResource
    BuiltinAnomaAction -> Str.anomaAction
    BuiltinAnomaComplianceInputs -> Str.anomaComplianceInputs
    BuiltinAnomaShieldedTransaction -> Str.anomaShieldedTransaction
    BuiltinNockmaNoun -> Str.nockmaNoun

instance Pretty BuiltinConstructor where
  pretty = \case
    BuiltinNatZero -> Str.zero
    BuiltinNatSuc -> Str.suc
    BuiltinBoolTrue -> Str.true
    BuiltinBoolFalse -> Str.false
    BuiltinIntOfNat -> Str.ofNat
    BuiltinIntNegSuc -> Str.negSuc
    BuiltinListNil -> Str.nil
    BuiltinListCons -> Str.cons
    BuiltinMaybeNothing -> Str.nothing
    BuiltinMaybeJust -> Str.just
    BuiltinPairConstr -> Str.pair
    BuiltinJsonArray -> Str.jsonArray
    BuiltinJsonBool -> Str.jsonBool
    BuiltinJsonObject -> Str.jsonObject
    BuiltinJsonNumber -> Str.jsonNumber
    BuiltinJsonString -> Str.jsonString
    BuiltinMkPoseidonState -> Str.cairoMkPoseidonState
    BuiltinMkEcPoint -> Str.cairoMkEcPoint
    BuiltinMkAnomaResource -> Str.anomaMkResource
    BuiltinMkAnomaAction -> Str.anomaMkAction
    BuiltinMkAnomaComplianceInputs -> Str.anomaMkComplianceInputs
    BuiltinMkAnomaShieldedTransaction -> Str.anomaMkShieldedTransaction
    BuiltinMkEq -> Str.mkEq
    BuiltinMkOrd -> Str.mkOrd
    BuiltinOrderingLT -> Str.lt
    BuiltinOrderingEQ -> Str.eq
    BuiltinOrderingGT -> Str.gt
    BuiltinNockmaAtom -> Str.nockmaAtom
    BuiltinNockmaCell -> Str.nockmaCell

data BuiltinConstructor
  = BuiltinNatZero
  | BuiltinNatSuc
  | BuiltinBoolTrue
  | BuiltinBoolFalse
  | BuiltinOrderingLT
  | BuiltinOrderingEQ
  | BuiltinOrderingGT
  | BuiltinIntOfNat
  | BuiltinIntNegSuc
  | BuiltinListNil
  | BuiltinListCons
  | BuiltinMkEq
  | BuiltinMkOrd
  | BuiltinMaybeNothing
  | BuiltinMaybeJust
  | BuiltinPairConstr
  | BuiltinJsonArray
  | BuiltinJsonBool
  | BuiltinJsonObject
  | BuiltinJsonNumber
  | BuiltinJsonString
  | BuiltinMkPoseidonState
  | BuiltinMkEcPoint
  | BuiltinMkAnomaResource
  | BuiltinMkAnomaAction
  | BuiltinMkAnomaComplianceInputs
  | BuiltinMkAnomaShieldedTransaction
  | BuiltinNockmaAtom
  | BuiltinNockmaCell
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinConstructor

instance Serialize BuiltinConstructor

instance NFData BuiltinConstructor

data BuiltinFunction
  = BuiltinAssert
  | BuiltinNatPlus
  | BuiltinNatSub
  | BuiltinNatMul
  | BuiltinNatUDiv
  | BuiltinNatDiv
  | BuiltinNatMod
  | BuiltinNatLe
  | BuiltinNatLt
  | BuiltinNatEq
  | BuiltinBoolIf
  | BuiltinBoolOr
  | BuiltinBoolAnd
  | BuiltinIntEq
  | BuiltinIntPlus
  | BuiltinIntSubNat
  | BuiltinIntNegNat
  | BuiltinIntNeg
  | BuiltinIntMul
  | BuiltinIntDiv
  | BuiltinIntMod
  | BuiltinIntSub
  | BuiltinIntNonNeg
  | BuiltinIntLe
  | BuiltinIntLt
  | BuiltinFromNat
  | BuiltinIsEqual
  | BuiltinOrdCompare
  | BuiltinFromInt
  | BuiltinSeq
  | BuiltinMonadBind
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinFunction

instance Serialize BuiltinFunction

instance NFData BuiltinFunction

instance Pretty BuiltinFunction where
  pretty = \case
    BuiltinAssert -> Str.assert
    BuiltinNatPlus -> Str.natPlus
    BuiltinNatSub -> Str.natSub
    BuiltinNatMul -> Str.natMul
    BuiltinNatUDiv -> Str.natUDiv
    BuiltinNatDiv -> Str.natDiv
    BuiltinNatMod -> Str.natMod
    BuiltinNatLe -> Str.natLe
    BuiltinNatLt -> Str.natLt
    BuiltinNatEq -> Str.natEq
    BuiltinBoolIf -> Str.boolIf
    BuiltinBoolOr -> Str.boolOr
    BuiltinBoolAnd -> Str.boolAnd
    BuiltinIntEq -> Str.intEq
    BuiltinIntPlus -> Str.intPlus
    BuiltinIntSubNat -> Str.intSubNat
    BuiltinIntNegNat -> Str.intNegNat
    BuiltinIntNeg -> Str.intNeg
    BuiltinIntMul -> Str.intMul
    BuiltinIntDiv -> Str.intDiv
    BuiltinIntMod -> Str.intMod
    BuiltinIntSub -> Str.intSub
    BuiltinIntNonNeg -> Str.intNonNeg
    BuiltinIntLe -> Str.intLe
    BuiltinIntLt -> Str.intLt
    BuiltinFromNat -> Str.fromNat
    BuiltinFromInt -> Str.fromInt
    BuiltinSeq -> Str.builtinSeq
    BuiltinIsEqual -> Str.isEqual
    BuiltinOrdCompare -> Str.compare
    BuiltinMonadBind -> Str.builtinMonadBind

data BuiltinAxiom
  = BuiltinNatPrint
  | BuiltinNatToString
  | BuiltinStringPrint
  | BuiltinStringConcat
  | BuiltinStringEq
  | BuiltinStringToNat
  | BuiltinBoolPrint
  | BuiltinFieldEq
  | BuiltinFieldAdd
  | BuiltinFieldSub
  | BuiltinFieldMul
  | BuiltinFieldDiv
  | BuiltinField
  | BuiltinFieldFromInt
  | BuiltinFieldToNat
  | BuiltinString
  | BuiltinIO
  | BuiltinIOSequence
  | BuiltinIOReadline
  | BuiltinTrace
  | BuiltinFail
  | BuiltinIntToString
  | BuiltinIntPrint
  | BuiltinAnomaGet
  | BuiltinAnomaEncode
  | BuiltinAnomaDecode
  | BuiltinAnomaVerifyDetached
  | BuiltinAnomaSign
  | BuiltinAnomaSignDetached
  | BuiltinAnomaVerifyWithMessage
  | BuiltinAnomaByteArrayToAnomaContents
  | BuiltinAnomaByteArrayFromAnomaContents
  | BuiltinAnomaSha256
  | BuiltinAnomaDelta
  | BuiltinAnomaKind
  | BuiltinAnomaResourceCommitment
  | BuiltinAnomaResourceNullifier
  | BuiltinAnomaResourceKind
  | BuiltinAnomaResourceDelta
  | BuiltinAnomaActionDelta
  | BuiltinAnomaActionsDelta
  | BuiltinAnomaZeroDelta
  | BuiltinAnomaAddDelta
  | BuiltinAnomaSubDelta
  | BuiltinAnomaRandomGenerator
  | BuiltinAnomaRandomGeneratorInit
  | BuiltinAnomaRandomNextBytes
  | BuiltinAnomaRandomSplit
  | BuiltinAnomaIsCommitment
  | BuiltinAnomaIsNullifier
  | BuiltinAnomaCreateFromComplianceInputs
  | BuiltinAnomaActionCreate
  | BuiltinAnomaTransactionCompose
  | BuiltinAnomaSet
  | BuiltinAnomaSetToList
  | BuiltinAnomaSetFromList
  | BuiltinPoseidon
  | BuiltinEcOp
  | BuiltinRandomEcPoint
  | BuiltinByte
  | BuiltinByteEq
  | BuiltinByteToNat
  | BuiltinByteFromNat
  | BuiltinByteArray
  | BuiltinByteArrayFromListByte
  | BuiltinByteArrayLength
  | BuiltinRangeCheck
  | BuiltinNockmaReify
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance HasNameKind BuiltinAxiom where
  getNameKind = \case
    BuiltinNatPrint -> KNameFunction
    BuiltinNatToString -> KNameFunction
    BuiltinStringPrint -> KNameFunction
    BuiltinStringConcat -> KNameFunction
    BuiltinStringEq -> KNameFunction
    BuiltinStringToNat -> KNameFunction
    BuiltinBoolPrint -> KNameFunction
    BuiltinFieldEq -> KNameFunction
    BuiltinFieldAdd -> KNameFunction
    BuiltinFieldMul -> KNameFunction
    BuiltinFieldSub -> KNameFunction
    BuiltinFieldDiv -> KNameFunction
    BuiltinField -> KNameInductive
    BuiltinFieldFromInt -> KNameFunction
    BuiltinFieldToNat -> KNameFunction
    BuiltinString -> KNameInductive
    BuiltinIO -> KNameInductive
    BuiltinIOSequence -> KNameFunction
    BuiltinIOReadline -> KNameFunction
    BuiltinTrace -> KNameFunction
    BuiltinFail -> KNameFunction
    BuiltinIntToString -> KNameFunction
    BuiltinIntPrint -> KNameFunction
    BuiltinAnomaGet -> KNameFunction
    BuiltinAnomaEncode -> KNameFunction
    BuiltinAnomaDecode -> KNameFunction
    BuiltinAnomaVerifyDetached -> KNameFunction
    BuiltinAnomaSign -> KNameFunction
    BuiltinAnomaSignDetached -> KNameFunction
    BuiltinAnomaVerifyWithMessage -> KNameFunction
    BuiltinAnomaByteArrayToAnomaContents -> KNameFunction
    BuiltinAnomaByteArrayFromAnomaContents -> KNameFunction
    BuiltinAnomaSha256 -> KNameFunction
    BuiltinAnomaDelta -> KNameInductive
    BuiltinAnomaKind -> KNameInductive
    BuiltinAnomaResourceCommitment -> KNameFunction
    BuiltinAnomaResourceNullifier -> KNameFunction
    BuiltinAnomaResourceKind -> KNameFunction
    BuiltinAnomaResourceDelta -> KNameFunction
    BuiltinAnomaActionDelta -> KNameFunction
    BuiltinAnomaActionsDelta -> KNameFunction
    BuiltinAnomaZeroDelta -> KNameFunction
    BuiltinAnomaAddDelta -> KNameFunction
    BuiltinAnomaSubDelta -> KNameFunction
    BuiltinAnomaRandomGenerator -> KNameInductive
    BuiltinAnomaRandomGeneratorInit -> KNameFunction
    BuiltinAnomaRandomNextBytes -> KNameFunction
    BuiltinAnomaRandomSplit -> KNameFunction
    BuiltinAnomaIsCommitment -> KNameFunction
    BuiltinAnomaIsNullifier -> KNameFunction
    BuiltinAnomaCreateFromComplianceInputs -> KNameFunction
    BuiltinAnomaActionCreate -> KNameFunction
    BuiltinAnomaTransactionCompose -> KNameFunction
    BuiltinPoseidon -> KNameFunction
    BuiltinEcOp -> KNameFunction
    BuiltinRandomEcPoint -> KNameFunction
    BuiltinByte -> KNameInductive
    BuiltinByteEq -> KNameFunction
    BuiltinByteToNat -> KNameFunction
    BuiltinByteFromNat -> KNameFunction
    BuiltinByteArray -> KNameInductive
    BuiltinByteArrayFromListByte -> KNameFunction
    BuiltinByteArrayLength -> KNameFunction
    BuiltinAnomaSet -> KNameInductive
    BuiltinAnomaSetToList -> KNameFunction
    BuiltinAnomaSetFromList -> KNameFunction
    BuiltinRangeCheck -> KNameFunction
    BuiltinNockmaReify -> KNameFunction
  getNameKindPretty :: BuiltinAxiom -> NameKind
  getNameKindPretty = getNameKind

instance Hashable BuiltinAxiom

instance Serialize BuiltinAxiom

instance NFData BuiltinAxiom

instance Pretty BuiltinAxiom where
  pretty = \case
    BuiltinNatPrint -> Str.natPrint
    BuiltinNatToString -> Str.natToString
    BuiltinStringPrint -> Str.stringPrint
    BuiltinStringConcat -> Str.stringConcat
    BuiltinStringEq -> Str.stringEq
    BuiltinStringToNat -> Str.stringToNat
    BuiltinFieldEq -> Str.fieldEq
    BuiltinFieldAdd -> Str.fieldAdd
    BuiltinFieldSub -> Str.fieldSub
    BuiltinFieldMul -> Str.fieldMul
    BuiltinFieldDiv -> Str.fieldDiv
    BuiltinField -> Str.field
    BuiltinFieldFromInt -> Str.fieldFromInt
    BuiltinFieldToNat -> Str.fieldToNat
    BuiltinBoolPrint -> Str.boolPrint
    BuiltinIO -> Str.io
    BuiltinString -> Str.string
    BuiltinIOSequence -> Str.ioSequence
    BuiltinIOReadline -> Str.ioReadline
    BuiltinTrace -> Str.trace_
    BuiltinFail -> Str.fail_
    BuiltinIntToString -> Str.intToString
    BuiltinIntPrint -> Str.intPrint
    BuiltinAnomaGet -> Str.anomaGet
    BuiltinAnomaEncode -> Str.anomaEncode
    BuiltinAnomaDecode -> Str.anomaDecode
    BuiltinAnomaVerifyDetached -> Str.anomaVerifyDetached
    BuiltinAnomaSignDetached -> Str.anomaSignDetached
    BuiltinAnomaSign -> Str.anomaSign
    BuiltinAnomaVerifyWithMessage -> Str.anomaVerifyWithMessage
    BuiltinAnomaByteArrayToAnomaContents -> Str.anomaByteArrayToAnomaContents
    BuiltinAnomaByteArrayFromAnomaContents -> Str.anomaByteArrayFromAnomaContents
    BuiltinAnomaSha256 -> Str.anomaSha256
    BuiltinAnomaDelta -> Str.anomaDelta
    BuiltinAnomaKind -> Str.anomaKind
    BuiltinAnomaResourceCommitment -> Str.anomaResourceCommitment
    BuiltinAnomaResourceNullifier -> Str.anomaResourceNullifier
    BuiltinAnomaResourceKind -> Str.anomaResourceKind
    BuiltinAnomaResourceDelta -> Str.anomaResourceDelta
    BuiltinAnomaActionDelta -> Str.anomaActionDelta
    BuiltinAnomaActionsDelta -> Str.anomaActionsDelta
    BuiltinAnomaZeroDelta -> Str.anomaZeroDelta
    BuiltinAnomaAddDelta -> Str.anomaAddDelta
    BuiltinAnomaSubDelta -> Str.anomaSubDelta
    BuiltinAnomaRandomGenerator -> Str.anomaRandomGenerator
    BuiltinAnomaRandomGeneratorInit -> Str.anomaRandomGeneratorInit
    BuiltinAnomaRandomNextBytes -> Str.anomaRandomNextBytes
    BuiltinAnomaRandomSplit -> Str.anomaRandomSplit
    BuiltinAnomaIsCommitment -> Str.anomaIsCommitment
    BuiltinAnomaIsNullifier -> Str.anomaIsNullifier
    BuiltinAnomaCreateFromComplianceInputs -> Str.anomaCreateFromComplianceInputs
    BuiltinAnomaTransactionCompose -> Str.anomaTransactionCompose
    BuiltinAnomaActionCreate -> Str.anomaActionCreate
    BuiltinAnomaSet -> Str.anomaSet
    BuiltinAnomaSetToList -> Str.anomaSetToList
    BuiltinAnomaSetFromList -> Str.anomaSetFromList
    BuiltinPoseidon -> Str.cairoPoseidon
    BuiltinEcOp -> Str.cairoEcOp
    BuiltinRandomEcPoint -> Str.cairoRandomEcPoint
    BuiltinByte -> Str.byte_
    BuiltinByteEq -> Str.byteEq
    BuiltinByteToNat -> Str.byteToNat
    BuiltinByteFromNat -> Str.byteFromNat
    BuiltinByteArray -> Str.byteArray
    BuiltinByteArrayFromListByte -> Str.byteArrayFromListByte
    BuiltinByteArrayLength -> Str.byteArrayLength
    BuiltinRangeCheck -> Str.rangeCheck
    BuiltinNockmaReify -> Str.nockmaReify

data BuiltinType
  = BuiltinTypeInductive BuiltinInductive
  | BuiltinTypeAxiom BuiltinAxiom
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinType

instance Serialize BuiltinType

instance NFData BuiltinType

instance Pretty BuiltinType where
  pretty = \case
    BuiltinTypeInductive ty -> pretty ty
    BuiltinTypeAxiom ax -> pretty ax

builtinTypeToPrim :: BuiltinType -> BuiltinPrim
builtinTypeToPrim = \case
  BuiltinTypeInductive b -> BuiltinsInductive b
  BuiltinTypeAxiom b -> BuiltinsAxiom b

isNatBuiltin :: BuiltinFunction -> Bool
isNatBuiltin = \case
  BuiltinNatPlus -> True
  BuiltinNatSub -> True
  BuiltinNatMul -> True
  BuiltinNatUDiv -> True
  BuiltinNatDiv -> True
  BuiltinNatMod -> True
  BuiltinNatLe -> True
  BuiltinNatLt -> True
  BuiltinNatEq -> True
  --
  BuiltinOrdCompare -> False
  BuiltinIsEqual -> False
  BuiltinAssert -> False
  BuiltinBoolIf -> False
  BuiltinBoolOr -> False
  BuiltinBoolAnd -> False
  BuiltinIntEq -> False
  BuiltinIntPlus -> False
  BuiltinIntSubNat -> False
  BuiltinIntNegNat -> False
  BuiltinIntNeg -> False
  BuiltinIntMul -> False
  BuiltinIntDiv -> False
  BuiltinIntMod -> False
  BuiltinIntSub -> False
  BuiltinIntNonNeg -> False
  BuiltinIntLe -> False
  BuiltinIntLt -> False
  BuiltinFromNat -> False
  BuiltinFromInt -> False
  BuiltinSeq -> False
  BuiltinMonadBind -> False

isIntBuiltin :: BuiltinFunction -> Bool
isIntBuiltin = \case
  BuiltinIntEq -> True
  BuiltinIntPlus -> True
  BuiltinIntSubNat -> True
  BuiltinIntNegNat -> True
  BuiltinIntNeg -> True
  BuiltinIntMul -> True
  BuiltinIntDiv -> True
  BuiltinIntMod -> True
  BuiltinIntSub -> True
  BuiltinIntNonNeg -> True
  BuiltinIntLe -> True
  BuiltinIntLt -> True
  --
  BuiltinAssert -> False
  BuiltinNatPlus -> False
  BuiltinNatSub -> False
  BuiltinNatMul -> False
  BuiltinNatUDiv -> False
  BuiltinNatDiv -> False
  BuiltinNatMod -> False
  BuiltinNatLe -> False
  BuiltinNatLt -> False
  BuiltinNatEq -> False
  BuiltinBoolIf -> False
  BuiltinBoolOr -> False
  BuiltinBoolAnd -> False
  BuiltinFromNat -> False
  BuiltinFromInt -> False
  BuiltinSeq -> False
  BuiltinIsEqual -> False
  BuiltinOrdCompare -> False
  BuiltinMonadBind -> False

isCastBuiltin :: BuiltinFunction -> Bool
isCastBuiltin = \case
  BuiltinFromNat -> True
  BuiltinFromInt -> True
  --
  BuiltinOrdCompare -> False
  BuiltinIsEqual -> False
  BuiltinAssert -> False
  BuiltinIntEq -> False
  BuiltinIntPlus -> False
  BuiltinIntSubNat -> False
  BuiltinIntNegNat -> False
  BuiltinIntNeg -> False
  BuiltinIntMul -> False
  BuiltinIntDiv -> False
  BuiltinIntMod -> False
  BuiltinIntSub -> False
  BuiltinIntNonNeg -> False
  BuiltinIntLe -> False
  BuiltinIntLt -> False
  BuiltinNatPlus -> False
  BuiltinNatSub -> False
  BuiltinNatMul -> False
  BuiltinNatUDiv -> False
  BuiltinNatDiv -> False
  BuiltinNatMod -> False
  BuiltinNatLe -> False
  BuiltinNatLt -> False
  BuiltinNatEq -> False
  BuiltinBoolIf -> False
  BuiltinBoolOr -> False
  BuiltinBoolAnd -> False
  BuiltinSeq -> False
  BuiltinMonadBind -> False

isIgnoredBuiltin :: BuiltinFunction -> Bool
isIgnoredBuiltin f
  | spec == explicit = spec
  | otherwise = impossible
  where
    spec :: Bool
    spec =
      (not . isNatBuiltin)
        .&&. (not . isIntBuiltin)
        .&&. (not . isCastBuiltin)
        .&&. (/= BuiltinMonadBind)
        .&&. (/= BuiltinIsEqual)
        .&&. (/= BuiltinOrdCompare)
        $ f

    explicit :: Bool
    explicit = case f of
      -- Cast
      BuiltinFromNat -> False
      BuiltinFromInt -> False
      -- Int
      BuiltinIntEq -> False
      BuiltinIntPlus -> False
      BuiltinIntSubNat -> False
      BuiltinIntNegNat -> False
      BuiltinIntNeg -> False
      BuiltinIntMul -> False
      BuiltinIntDiv -> False
      BuiltinIntMod -> False
      BuiltinIntSub -> False
      BuiltinIntNonNeg -> False
      BuiltinIntLe -> False
      BuiltinIntLt -> False
      -- Nat
      BuiltinNatPlus -> False
      BuiltinNatSub -> False
      BuiltinNatMul -> False
      BuiltinNatUDiv -> False
      BuiltinNatDiv -> False
      BuiltinNatMod -> False
      BuiltinNatLe -> False
      BuiltinNatLt -> False
      BuiltinNatEq -> False
      -- Eq
      BuiltinIsEqual -> False
      -- Ord
      BuiltinOrdCompare -> False
      -- Monad
      BuiltinMonadBind -> False
      -- Ignored
      BuiltinAssert -> True
      BuiltinBoolIf -> True
      BuiltinBoolOr -> True
      BuiltinBoolAnd -> True
      BuiltinSeq -> True
