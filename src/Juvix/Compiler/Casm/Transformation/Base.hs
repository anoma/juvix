module Juvix.Compiler.Casm.Transformation.Base
  ( module Juvix.Compiler.Casm.Transformation.Base,
    module Juvix.Compiler.Casm.Language,
    module Juvix.Compiler.Tree.Options,
  )
where

import Juvix.Compiler.Casm.Language
import Juvix.Compiler.Tree.Options

mapT :: ([Instruction] -> [Instruction]) -> [Instruction] -> [Instruction]
mapT f = go
  where
    go :: [Instruction] -> [Instruction]
    go = \case
      i : is -> f (i : go is)
      [] -> f []
