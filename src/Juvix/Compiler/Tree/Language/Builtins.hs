module Juvix.Compiler.Tree.Language.Builtins where

import Juvix.Prelude

data BinaryOp
  = OpIntAdd
  | OpIntSub
  | OpIntMul
  | OpIntDiv
  | OpIntMod
  | OpIntLt
  | OpIntLe
  | OpEq
  | OpStrConcat
  deriving stock (Eq)

data UnaryOp
  = -- | Convert the argument to a string. JV* opcode: `show`.
    OpShow
  | -- | Convert a string to an integer. JV* opcode: `atoi`.
    OpStrToInt
  | -- | Compute the number of expected arguments for the given closure. JV*
    -- opcode: `argsnum`.
    OpArgsNum
  deriving stock (Eq)
