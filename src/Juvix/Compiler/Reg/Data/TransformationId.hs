module Juvix.Compiler.Reg.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Reg.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = IdentityTrans
  | CleanupCairo
  | Cleanup
  | SSA
  | InitBranchVars
  | CopyPropagation
  | ConstantPropagation
  | DeadCodeElimination
  | BranchToIf
  | BranchOnZeroToIf
  | OptPhaseMain
  | OptPhaseCairo
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineCasm
  | PipelineC
  | PipelineRust
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toCTransformations :: [TransformationId]
toCTransformations = [Cleanup]

toRustTransformations :: [TransformationId]
toRustTransformations = [Cleanup]

toCasmTransformations :: [TransformationId]
toCasmTransformations = [CleanupCairo, SSA, OptPhaseCairo]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    IdentityTrans -> strIdentity
    CleanupCairo -> strCleanupCairo
    Cleanup -> strCleanup
    SSA -> strSSA
    InitBranchVars -> strInitBranchVars
    CopyPropagation -> strCopyPropagation
    ConstantPropagation -> strConstantPropagation
    DeadCodeElimination -> strDeadCodeElimination
    BranchToIf -> strBranchToIf
    BranchOnZeroToIf -> strBranchOnZeroToIf
    OptPhaseMain -> strOptPhaseMain
    OptPhaseCairo -> strOptPhaseCairo

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineC -> strCPipeline
    PipelineRust -> strRustPipeline
    PipelineCasm -> strCasmPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineC -> toCTransformations
    PipelineRust -> toRustTransformations
    PipelineCasm -> toCasmTransformations
