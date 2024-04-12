module Juvix.Compiler.Casm.Data.Builtins where

import Juvix.Extra.Strings qualified as Str
import Juvix.Prelude

-- The order of the builtins must correspond to the "standard" builtin order in
-- the Cairo VM implementation. See:
-- https://github.com/lambdaclass/cairo-vm/blob/main/vm/src/vm/runners/cairo_runner.rs#L257
data Builtin
  = BuiltinRangeCheck
  | BuiltinEcOp
  | BuiltinPoseidon
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance Hashable Builtin

builtinsNum :: Int
builtinsNum = length (allElements @Builtin)

builtinName :: Builtin -> Text
builtinName = \case
  BuiltinRangeCheck -> Str.cairoRangeCheck
  BuiltinEcOp -> Str.cairoEcOp
  BuiltinPoseidon -> Str.cairoPoseidon

builtinNames :: [Text]
builtinNames = map builtinName allElements
