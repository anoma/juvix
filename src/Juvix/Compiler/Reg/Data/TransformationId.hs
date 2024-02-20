module Juvix.Compiler.Reg.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Reg.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = Identity
  | SSA
  | Cleanup
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineCairo
  | PipelineC
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toCTransformations :: [TransformationId]
toCTransformations = [Cleanup]

toCairoTransformations :: [TransformationId]
toCairoTransformations = [Cleanup, SSA]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    Identity -> strIdentity
    SSA -> strSSA
    Cleanup -> strCleanup

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineC -> strCPipeline
    PipelineCairo -> strCairoPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineC -> toCTransformations
    PipelineCairo -> toCairoTransformations
