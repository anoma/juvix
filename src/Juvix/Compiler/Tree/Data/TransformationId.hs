module Juvix.Compiler.Tree.Data.TransformationId where

import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Compiler.Tree.Data.TransformationId.Strings
import Juvix.Prelude

data TransformationId
  = IdentityTrans
  | IdentityU
  | IdentityD
  | ConvertUnaryCalls
  | OptPhaseMain
  | Apply
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
toNockmaTransformations = [Validate, OptPhaseMain, Apply, FilterUnreachable]

toAsmTransformations :: [TransformationId]
toAsmTransformations = [Validate, CheckNoAnoma, CheckNoByteArray, OptPhaseMain]

toCairoAsmTransformations :: [TransformationId]
toCairoAsmTransformations = [Validate, OptPhaseMain, Apply, FilterUnreachable]

instance TransformationId' TransformationId where
  transformationText :: TransformationId -> Text
  transformationText = \case
    IdentityTrans -> strIdentity
    IdentityU -> strIdentityU
    IdentityD -> strIdentityD
    ConvertUnaryCalls -> strConvertUnaryCalls
    OptPhaseMain -> strOptPhaseMain
    Apply -> strApply
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
