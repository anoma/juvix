module Juvix.Compiler.Core.Pipeline
  ( module Juvix.Compiler.Core.Pipeline,
    module Juvix.Compiler.Core.Data.InfoTable,
  )
where

import Juvix.Compiler.Core.Data.InfoTable
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Core.Transformation
import Juvix.Compiler.Pipeline.EntryPoint (EntryPoint)

toTypechecked :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toTypechecked = mapReader fromEntryPoint . applyTransformations toTypecheckTransformations

-- | Perform transformations on Core necessary for storage
toStored :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toStored = mapReader fromEntryPoint . applyTransformations toStoredTransformations

-- | Perform transformations on stored Core necessary before the translation to
-- Core.Stripped
toStripped :: (Members '[Error JuvixError, Reader EntryPoint] r) => TransformationId -> Module -> Sem r Module
toStripped checkId = mapReader fromEntryPoint . applyTransformations (toStrippedTransformations checkId)

-- | Perform transformations on stored Core necessary before the translation to VampIR
toVampIR :: (Members '[Error JuvixError, Reader EntryPoint] r) => Module -> Sem r Module
toVampIR = mapReader fromEntryPoint . applyTransformations toVampIRTransformations

extraAnomaTransformations :: [TransformationId]
extraAnomaTransformations = [ComputeCaseANF]

applyExtraTransformations :: (Members '[Error JuvixError, Reader EntryPoint] r) => [TransformationId] -> Module -> Sem r Module
applyExtraTransformations transforms = mapReader fromEntryPoint . applyTransformations transforms
