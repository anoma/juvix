module Juvix.Compiler.Concrete.Data.Builtins where

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

data BuiltinInductive
  = BuiltinNat
  | BuiltinBool
  | BuiltinInt
  | BuiltinList
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinInductive

instance Pretty BuiltinInductive where
  pretty = \case
    BuiltinNat -> Str.nat
    BuiltinBool -> Str.bool_
    BuiltinInt -> Str.int_
    BuiltinList -> Str.list

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

data BuiltinConstructor
  = BuiltinNatZero
  | BuiltinNatSuc
  | BuiltinBoolTrue
  | BuiltinBoolFalse
  | BuiltinIntOfNat
  | BuiltinIntNegSuc
  | BuiltinListNil
  | BuiltinListCons
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinConstructor

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
  | BuiltinSeq
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinFunction

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
    BuiltinSeq -> Str.builtinSeq

data BuiltinAxiom
  = BuiltinNatPrint
  | BuiltinNatToString
  | BuiltinStringPrint
  | BuiltinStringConcat
  | BuiltinStringEq
  | BuiltinStringToNat
  | BuiltinBoolPrint
  | BuiltinString
  | BuiltinIO
  | BuiltinIOSequence
  | BuiltinIOReadline
  | BuiltinTrace
  | BuiltinFail
  | BuiltinIntToString
  | BuiltinIntPrint
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinAxiom

instance Pretty BuiltinAxiom where
  pretty = \case
    BuiltinNatPrint -> Str.natPrint
    BuiltinNatToString -> Str.natToString
    BuiltinStringPrint -> Str.stringPrint
    BuiltinStringConcat -> Str.stringConcat
    BuiltinStringEq -> Str.stringEq
    BuiltinStringToNat -> Str.stringToNat
    BuiltinBoolPrint -> Str.boolPrint
    BuiltinIO -> Str.io
    BuiltinString -> Str.string
    BuiltinIOSequence -> Str.ioSequence
    BuiltinIOReadline -> Str.ioReadline
    BuiltinTrace -> Str.trace_
    BuiltinFail -> Str.fail_
    BuiltinIntToString -> Str.intToString
    BuiltinIntPrint -> Str.intPrint

data BuiltinType
  = BuiltinTypeInductive BuiltinInductive
  | BuiltinTypeAxiom BuiltinAxiom
  deriving stock (Show, Eq, Ord, Generic, Data)

instance Hashable BuiltinType

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

isIgnoredBuiltin :: BuiltinFunction -> Bool
isIgnoredBuiltin f = not (isNatBuiltin f) && not (isIntBuiltin f)
