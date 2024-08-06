module Juvix.Compiler.Tree.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Tree.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = IdentityTrans
  | IdentityU
  | IdentityD
  | Apply
  | TempHeight
  | FilterUnreachable
  | Validate
  | CheckNoAnoma
  | CheckNoByteArray
  deriving stock (Data, Bounded, Enum, Show)

data PipelineId
  = PipelineNockma
  | PipelineAsm
  | PipelineCairoAsm
  deriving stock (Data, Bounded, Enum)

type TransformationLikeId = TransformationLikeId' TransformationId PipelineId

toNockmaTransformations :: [TransformationId]
toNockmaTransformations = [Validate, Apply, FilterUnreachable, TempHeight]

toAsmTransformations :: [TransformationId]
toAsmTransformations = [Validate, CheckNoAnoma, CheckNoByteArray]

toCairoAsmTransformations :: [TransformationId]
toCairoAsmTransformations = [Validate, Apply, FilterUnreachable]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    IdentityTrans -> strIdentity
    IdentityU -> strIdentityU
    IdentityD -> strIdentityD
    Apply -> strApply
    TempHeight -> strTempHeight
    FilterUnreachable -> strFilterUnreachable
    Validate -> strValidate
    CheckNoAnoma -> strCheckNoAnoma
    CheckNoByteArray -> strCheckNoByteArray

instance PipelineId' TransformationId PipelineId where
  pipelineText :: PipelineId -> Text
  pipelineText = \case
    PipelineNockma -> strNockmaPipeline
    PipelineAsm -> strAsmPipeline
    PipelineCairoAsm -> strCairoAsmPipeline

  pipeline :: PipelineId -> [TransformationId]
  pipeline = \case
    PipelineNockma -> toNockmaTransformations
    PipelineAsm -> toAsmTransformations
    PipelineCairoAsm -> toCairoAsmTransformations
