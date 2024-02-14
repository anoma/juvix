module Juvix.Compiler.Reg.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Reg.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = Identity
  | Cleanup
  | SSA
  | InitBranchVars
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineCairo
  | PipelineC
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toCTransformations :: [TransformationId]
toCTransformations = [Cleanup]

toCairoTransformations :: [TransformationId]
toCairoTransformations = [Cleanup, SSA, InitBranchVars]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    Identity -> strIdentity
    Cleanup -> strCleanup
    SSA -> strSSA
    InitBranchVars -> strInitBranchVars

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineC -> strCPipeline
    PipelineCairo -> strCairoPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineC -> toCTransformations
    PipelineCairo -> toCairoTransformations
