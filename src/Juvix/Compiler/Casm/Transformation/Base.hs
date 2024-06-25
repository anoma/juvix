module Juvix.Compiler.Casm.Transformation.Base where

import Juvix.Compiler.Casm.Language

mapT :: ([Instruction] -> [Instruction]) -> [Instruction] -> [Instruction]
mapT f = go
  where
    go :: [Instruction] -> [Instruction]
    go = \case
      i : is -> f (i : go is)
      [] -> f []
