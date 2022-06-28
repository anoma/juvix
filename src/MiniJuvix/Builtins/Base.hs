module MiniJuvix.Builtins.Base
  ( module MiniJuvix.Builtins.Base,
  )
where

import MiniJuvix.Internal.Strings qualified as Str
import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty

data BuiltinsEnum
  = BuiltinsNatural
  | BuiltinsZero
  | BuiltinsSuc
  | BuiltinsNaturalPlus
  | BuiltinsNaturalPrint
  | BuiltinsIO
  | BuiltinsIOSequence
  deriving stock (Show, Eq, Generic)

instance Hashable BuiltinsEnum

instance Pretty BuiltinsEnum where
  pretty = \case
    BuiltinsNatural -> Str.natural
    BuiltinsZero -> "zero"
    BuiltinsSuc -> "suc"
    BuiltinsNaturalPlus -> Str.naturalPlus
    BuiltinsNaturalPrint -> Str.naturalPrint
    BuiltinsIO -> Str.io
    BuiltinsIOSequence -> Str.ioSequence
