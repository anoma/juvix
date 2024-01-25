module Juvix.Compiler.Tree.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Tree.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = Identity
  | IdentityU
  | IdentityD
  | Apply
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineNockma
  | PipelineAsm
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toNockmaTransformations :: [TransformationId]
toNockmaTransformations = [Apply]

toAsmTransformations :: [TransformationId]
toAsmTransformations = []

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    Identity -> strIdentity
    IdentityU -> strIdentityU
    IdentityD -> strIdentityD
    Apply -> strApply

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineNockma -> strNockmaPipeline
    PipelineAsm -> strAsmPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineNockma -> toNockmaTransformations
    PipelineAsm -> toAsmTransformations
