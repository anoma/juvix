module Juvix.Compiler.Concrete.Data.Builtins where

import Data.Serialize
import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

class IsBuiltin a where
  toBuiltinPrim :: a -> BuiltinPrim

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

instance Pretty BuiltinPrim where
  pretty = \case
    BuiltinsInductive i -> pretty i
    BuiltinsConstructor c -> pretty c
    BuiltinsFunction f -> pretty f
    BuiltinsAxiom a -> pretty a

builtinConstructors :: BuiltinInductive -> [BuiltinConstructor]
builtinConstructors = \case
  BuiltinNat -> [BuiltinNatZero, BuiltinNatSuc]
  BuiltinBool -> [BuiltinBoolTrue, BuiltinBoolFalse]
  BuiltinInt -> [BuiltinIntOfNat, BuiltinIntNegSuc]
  BuiltinList -> [BuiltinListNil, BuiltinListCons]
  BuiltinPoseidonState -> [BuiltinMkPoseidonState]
  BuiltinEcPoint -> [BuiltinMkEcPoint]

data BuiltinInductive
  = BuiltinNat
  | BuiltinBool
  | BuiltinInt
  | BuiltinList
  | BuiltinPoseidonState
  | BuiltinEcPoint
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinInductive

instance Serialize BuiltinInductive

instance Pretty BuiltinInductive where
  pretty = \case
    BuiltinNat -> Str.nat
    BuiltinBool -> Str.bool_
    BuiltinInt -> Str.int_
    BuiltinList -> Str.list
    BuiltinPoseidonState -> Str.cairoPoseidonState
    BuiltinEcPoint -> Str.cairoEcPoint

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
    BuiltinMkPoseidonState -> Str.cairoMkPoseidonState
    BuiltinMkEcPoint -> Str.cairoMkEcPoint

data BuiltinConstructor
  = BuiltinNatZero
  | BuiltinNatSuc
  | BuiltinBoolTrue
  | BuiltinBoolFalse
  | BuiltinIntOfNat
  | BuiltinIntNegSuc
  | BuiltinListNil
  | BuiltinListCons
  | BuiltinMkPoseidonState
  | BuiltinMkEcPoint
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinConstructor

instance Serialize BuiltinConstructor

data BuiltinFunction
  = BuiltinNatPlus
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
  | BuiltinFromInt
  | BuiltinSeq
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinFunction

instance Serialize BuiltinFunction

instance Pretty BuiltinFunction where
  pretty = \case
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
  | BuiltinPoseidon
  | BuiltinEcOp
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinAxiom

instance Serialize BuiltinAxiom

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
    BuiltinPoseidon -> Str.cairoPoseidon
    BuiltinEcOp -> Str.cairoEcOp

data BuiltinType
  = BuiltinTypeInductive BuiltinInductive
  | BuiltinTypeAxiom BuiltinAxiom
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinType

instance Serialize BuiltinType

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
  _ -> False

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
  _ -> False

isCastBuiltin :: BuiltinFunction -> Bool
isCastBuiltin = \case
  BuiltinFromNat -> True
  BuiltinFromInt -> True
  _ -> False

isIgnoredBuiltin :: BuiltinFunction -> Bool
isIgnoredBuiltin f = not (isNatBuiltin f) && not (isIntBuiltin f) && not (isCastBuiltin f)
