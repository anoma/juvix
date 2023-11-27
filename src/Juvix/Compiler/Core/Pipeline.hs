module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Pipeline.EntryPoint (EntryPoint)

-- | Perform transformations on Core necessary for storage
toStored' :: (Members '[Error JuvixError, Reader CoreOptions] r) => Module -> Sem r Module
toStored' = applyTransformations toStoredTransformations

toTypechecked :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toTypechecked = mapReader fromEntryPoint . applyTransformations toTypecheckTransformations

toStored :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toStored = mapReader fromEntryPoint . applyTransformations toStoredTransformations

-- | Perform transformations on stored Core necessary before the translation to
-- Core.Stripped
toStripped' :: (Members '[Error JuvixError, Reader CoreOptions] r) => Module -> Sem r Module
toStripped' = applyTransformations toStrippedTransformations

toStripped :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toStripped = mapReader fromEntryPoint . applyTransformations toStrippedTransformations

-- | Perform transformations on stored Core necessary before the translation to GEB
toGeb' :: (Members '[Error JuvixError, Reader CoreOptions] r) => Module -> Sem r Module
toGeb' = applyTransformations toGebTransformations

toGeb :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toGeb = mapReader fromEntryPoint . applyTransformations toGebTransformations

-- | Perform transformations on stored Core necessary before the translation to VampIR
toVampIR' :: (Members '[Error JuvixError, Reader CoreOptions] r) => Module -> Sem r Module
toVampIR' = applyTransformations toVampIRTransformations

toVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toVampIR = mapReader fromEntryPoint . applyTransformations toVampIRTransformations
