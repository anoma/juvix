module Juvix.Compiler.Core.Extra.Recursors.Recur where

import Juvix.Compiler.Core.Language

data Recur' c
  = End' Node
  | Recur' (c, Node)

data Recur
  = End Node
  | Recur Node

{- recurNode :: Lens' (Recur c) (c, Node)
recurNode f = \case
  End n -> End <$> f n
  Recur n -> Recur <$> f n
 -}
