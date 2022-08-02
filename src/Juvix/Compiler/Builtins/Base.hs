module Juvix.Compiler.Builtins.Base
  ( module Juvix.Compiler.Builtins.Base,
  )
where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude
import Juvix.Prelude.Pretty

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
