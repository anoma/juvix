module Juvix.Compiler.Casm.Data.Builtins where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

data Builtin
  = BuiltinRangeCheck
  | BuiltinPoseidon
  | BuiltinEcOp
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance Hashable Builtin

builtinsNum :: Int
builtinsNum = length (allElements @Builtin)

builtinName :: Builtin -> Text
builtinName = \case
  BuiltinRangeCheck -> Str.cairoRangeCheck
  BuiltinPoseidon -> Str.cairoPoseidon
  BuiltinEcOp -> Str.cairoEcOp

builtinNames :: [Text]
builtinNames = map builtinName allElements
