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
    BuiltinsConstructor {} -> impossible
    BuiltinsFunction f -> pretty f
    BuiltinsAxiom a -> pretty a

builtinConstructors :: BuiltinInductive -> [BuiltinConstructor]
builtinConstructors = \case
  BuiltinNat -> [BuiltinNatZero, BuiltinNatSuc]
  BuiltinBool -> [BuiltinBoolTrue, BuiltinBoolFalse]

data BuiltinInductive
  = BuiltinNat
  | BuiltinBool
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinInductive

instance Pretty BuiltinInductive where
  pretty = \case
    BuiltinNat -> Str.nat
    BuiltinBool -> Str.bool_

data BuiltinConstructor
  = BuiltinNatZero
  | BuiltinNatSuc
  | BuiltinBoolTrue
  | BuiltinBoolFalse
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

data BuiltinAxiom
  = BuiltinNatPrint
  | BuiltinStringPrint
  | BuiltinBoolPrint
  | BuiltinString
  | BuiltinIO
  | BuiltinIOSequence
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Data)

instance Hashable BuiltinAxiom

instance Pretty BuiltinAxiom where
  pretty = \case
    BuiltinNatPrint -> Str.natPrint
    BuiltinStringPrint -> Str.stringPrint
    BuiltinBoolPrint -> Str.boolPrint
    BuiltinIO -> Str.io
    BuiltinString -> Str.string
    BuiltinIOSequence -> Str.ioSequence
