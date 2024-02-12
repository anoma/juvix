module Juvix.Compiler.Reg.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Reg.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = Identity
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineC
  | PipelineCairo
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toCTransformations :: [TransformationId]
toCTransformations = []

toCairoTransformations :: [TransformationId]
toCairoTransformations = []

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    Identity -> strIdentity

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineC -> strCPipeline
    PipelineCairo -> strCairoPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineC -> toCTransformations
    PipelineCairo -> toCairoTransformations
