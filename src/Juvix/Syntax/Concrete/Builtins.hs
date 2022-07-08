module Juvix.Syntax.Concrete.Builtins where

import Juvix.Internal.Strings qualified as Str
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
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable BuiltinPrim

instance Pretty BuiltinPrim where
  pretty = \case
    BuiltinsInductive i -> pretty i
    BuiltinsConstructor {} -> impossible
    BuiltinsFunction f -> pretty f
    BuiltinsAxiom a -> pretty a

builtinConstructors :: BuiltinInductive -> [BuiltinConstructor]
builtinConstructors = \case
  BuiltinNatural -> [BuiltinNaturalZero, BuiltinNaturalSuc]

data BuiltinInductive
  = BuiltinNatural
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable BuiltinInductive

instance Pretty BuiltinInductive where
  pretty = \case
    BuiltinNatural -> Str.natural

data BuiltinConstructor
  = BuiltinNaturalZero
  | BuiltinNaturalSuc
  deriving stock (Show, Eq, Ord, Generic)

instance Hashable BuiltinConstructor

data BuiltinFunction
  = BuiltinNaturalPlus
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable BuiltinFunction

instance Pretty BuiltinFunction where
  pretty = \case
    BuiltinNaturalPlus -> Str.naturalPlus

data BuiltinAxiom
  = BuiltinNaturalPrint
  | BuiltinIO
  | BuiltinIOSequence
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable BuiltinAxiom

instance Pretty BuiltinAxiom where
  pretty = \case
    BuiltinNaturalPrint -> Str.naturalPrint
    BuiltinIO -> Str.io
    BuiltinIOSequence -> Str.ioSequence
