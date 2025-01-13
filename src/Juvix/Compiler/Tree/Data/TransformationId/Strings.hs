module Juvix.Compiler.Tree.Data.TransformationId.Strings where

import Juvix.Prelude

strNockmaPipeline :: Text
strNockmaPipeline = "pipeline-nockma"

strAsmPipeline :: Text
strAsmPipeline = "pipeline-asm"

strCairoAsmPipeline :: Text
strCairoAsmPipeline = "pipeline-cairo-asm"

strIdentity :: Text
strIdentity = "identity"

strIdentityU :: Text
strIdentityU = "identity-umap"

strIdentityD :: Text
strIdentityD = "identity-dmap"

strConvertUnaryCalls :: Text
strConvertUnaryCalls = "convert-unary-calls"

strOptPhaseMain :: Text
strOptPhaseMain = "opt-phase-main"

strApply :: Text
strApply = "apply"

strFilterUnreachable :: Text
strFilterUnreachable = "filter-unreachable"

strValidate :: Text
strValidate = "validate"

strCheckNoAnoma :: Text
strCheckNoAnoma = "check-no-anoma"

strCheckNoByteArray :: Text
strCheckNoByteArray = "check-no-bytearray"
