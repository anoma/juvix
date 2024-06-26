module Juvix.Compiler.Casm.Data.TransformationId where

import Juvix.Compiler.Casm.Data.TransformationId.Strings
import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Prelude

data TransformationId
  = IdentityTrans
  | Peephole
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineCairo
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toCairoTransformations :: [TransformationId]
toCairoTransformations = [Peephole]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    IdentityTrans -> strIdentity
    Peephole -> strPeephole

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineCairo -> strCairoPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineCairo -> toCairoTransformations
