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
    BuiltinsNatPrint -> Str.natPrint
    BuiltinsIO -> Str.io
    BuiltinsIOSequence -> Str.ioSequence
