module Juvix.Compiler.Reg.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Reg.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = IdentityTrans
  | Cleanup
  | SSA
  | InitBranchVars
  | CopyPropagation
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
toCasmTransformations = [Cleanup, CopyPropagation, SSA]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    IdentityTrans -> strIdentity
    Cleanup -> strCleanup
    SSA -> strSSA
    InitBranchVars -> strInitBranchVars
    CopyPropagation -> strCopyPropagation

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
