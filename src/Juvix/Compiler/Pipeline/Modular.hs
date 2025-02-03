module Juvix.Compiler.Pipeline.Modular where

import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Core.Translation.Stripped.FromCore qualified as Stripped
import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Tree qualified as Tree
import Juvix.Prelude

storedCoreToTree ::
  (Members '[Error JuvixError, Reader EntryPoint] r) =>
  Core.TransformationId ->
  [Core.TransformationId] ->
  Core.Module ->
  Sem r Tree.InfoTable
storedCoreToTree checkId extraTransforms md = do
  fsize <- asks (^. entryPointFieldSize)
  Tree.fromCore
    . Stripped.fromCore fsize
    . Core.computeCombinedInfoTable
    <$> (Core.toStripped checkId md >>= Core.applyExtraTransformations extraTransforms)
