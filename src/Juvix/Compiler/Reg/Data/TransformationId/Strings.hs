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

strConstantPropagation :: Text
strConstantPropagation = "constant-propagation"

strDeadCodeElimination :: Text
strDeadCodeElimination = "dead-code"

strBranchToIf :: Text
strBranchToIf = "branch-to-if"

strBranchOnZeroToIf :: Text
strBranchOnZeroToIf = "branch-on-zero-to-if"

strOptPhaseMain :: Text
strOptPhaseMain = "opt-main"

strOptPhaseCairo :: Text
strOptPhaseCairo = "opt-cairo"
