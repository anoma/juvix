module Juvix.Compiler.Core.Extra.Recursors.Generic.Recur where

data Recur' n c
  = End' n
  | Recur' (c, n)

data Recur n
  = End n
  | Recur n
