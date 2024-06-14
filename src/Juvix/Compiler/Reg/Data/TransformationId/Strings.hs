module Juvix.Compiler.Reg.Data.TransformationId.Strings where

import Juvix.Prelude

strCPipeline :: Text
strCPipeline = "pipeline-c"

strRustPipeline :: Text
strRustPipeline = "pipeline-rust"

strCasmPipeline :: Text
strCasmPipeline = "pipeline-casm"

strIdentity :: Text
strIdentity = "identity"

strCleanup :: Text
strCleanup = "cleanup"

strSSA :: Text
strSSA = "ssa"

strInitBranchVars :: Text
strInitBranchVars = "init-branch-vars"

strCopyPropagation :: Text
strCopyPropagation = "copy-propagation"
