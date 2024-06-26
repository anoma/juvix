module Juvix.Compiler.Casm.Pipeline
  ( module Juvix.Compiler.Casm.Pipeline,
    Options,
    Code,
  )
where

import Juvix.Compiler.Casm.Transformation
import Juvix.Compiler.Pipeline.EntryPoint (EntryPoint)

-- | Perform transformations on CASM necessary before the translation to Cairo
-- bytecode
toCairo' :: Code -> Sem r Code
toCairo' = applyTransformations toCairoTransformations

toCairo :: (Member (Reader EntryPoint) r) => Code -> Sem r Code
toCairo = mapReader fromEntryPoint . toCairo'
