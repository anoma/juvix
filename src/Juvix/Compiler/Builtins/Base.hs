module Juvix.Compiler.Builtins.Base
  ( module Juvix.Compiler.Builtins.Base,
  )
where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

data BuiltinsEnum
  = BuiltinsNat
  | BuiltinsZero
  | BuiltinsSuc
  | BuiltinsNatPlus
  | BuiltinsNatMul
  | BuiltinsNatDiv
  | BuiltinsNatMod
  | BuiltinsNatLe
  | BuiltinsNatLt
  | BuiltinsNatEq
  | BuiltinsNatPrint
  | BuiltinsIO
  | BuiltinsIOSequence
  deriving stock (Show, Eq, Generic)

instance Hashable BuiltinsEnum

instance Pretty BuiltinsEnum where
  pretty = \case
    BuiltinsNat -> Str.nat
    BuiltinsZero -> "zero"
    BuiltinsSuc -> "suc"
    BuiltinsNatPlus -> Str.natPlus
    BuiltinsNatMul -> Str.natMul
    BuiltinsNatDiv -> Str.natDiv
    BuiltinsNatMod -> Str.natMod
    BuiltinsNatLe -> Str.natLe
    BuiltinsNatLt -> Str.natLt
    BuiltinsNatEq -> Str.natEq
    BuiltinsNatPrint -> Str.natPrint
    BuiltinsIO -> Str.io
    BuiltinsIOSequence -> Str.ioSequence
