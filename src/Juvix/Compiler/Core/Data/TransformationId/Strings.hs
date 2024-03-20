module Juvix.Compiler.Core.Data.TransformationId.Strings where

import Juvix.Prelude

strLetHoisting :: Text
strLetHoisting = "let-hoisting"

strStoredPipeline :: Text
strStoredPipeline = "pipeline-stored"

strNormalizePipeline :: Text
strNormalizePipeline = "pipeline-normalize"

strGebPipeline :: Text
strGebPipeline = "pipeline-geb"

strVampIRPipeline :: Text
strVampIRPipeline = "pipeline-vampir"

strStrippedPipeline :: Text
strStrippedPipeline = "pipeline-stripped"

strExecPipeline :: Text
strExecPipeline = "pipeline-exec"

strLifting :: Text
strLifting = "lifting"

strLetRecLifting :: Text
strLetRecLifting = "letrec-lifting"

strTopEtaExpand :: Text
strTopEtaExpand = "top-eta-expand"

strMatchToCase :: Text
strMatchToCase = "match-to-case"

strNaiveMatchToCase :: Text
strNaiveMatchToCase = "naive-match-to-case"

strEtaExpandApps :: Text
strEtaExpandApps = "eta-expand-apps"

strIdentity :: Text
strIdentity = "identity"

strRemoveTypeArgs :: Text
strRemoveTypeArgs = "remove-type-args"

strMoveApps :: Text
strMoveApps = "move-apps"

strNatToPrimInt :: Text
strNatToPrimInt = "nat-to-primint"

strIntToPrimInt :: Text
strIntToPrimInt = "int-to-primint"

strConvertBuiltinTypes :: Text
strConvertBuiltinTypes = "convert-builtin-types"

strComputeTypeInfo :: Text
strComputeTypeInfo = "compute-type-info"

strUnrollRecursion :: Text
strUnrollRecursion = "unroll-recursion"

strDisambiguateNames :: Text
strDisambiguateNames = "disambiguate-names"

strCombineInfoTables :: Text
strCombineInfoTables = "combine-info-tables"

strCheckGeb :: Text
strCheckGeb = "check-geb"

strCheckExec :: Text
strCheckExec = "check-exec"

strCheckVampIR :: Text
strCheckVampIR = "check-vampir"

strCheckAnoma :: Text
strCheckAnoma = "check-anoma"

strCheckCairo :: Text
strCheckCairo = "check-cairo"

strNormalize :: Text
strNormalize = "normalize"

strLetFolding :: Text
strLetFolding = "let-folding"

strLambdaFolding :: Text
strLambdaFolding = "lambda-folding"

strInlining :: Text
strInlining = "inlining"

strMandatoryInlining :: Text
strMandatoryInlining = "mandatory-inlining"

strFoldTypeSynonyms :: Text
strFoldTypeSynonyms = "fold-type-synonyms"

strCaseCallLifting :: Text
strCaseCallLifting = "case-call-lifting"

strSimplifyIfs :: Text
strSimplifyIfs = "simplify-ifs"

strSimplifyComparisons :: Text
strSimplifyComparisons = "simplify-comparisons"

strSpecializeArgs :: Text
strSpecializeArgs = "specialize-args"

strCaseFolding :: Text
strCaseFolding = "case-folding"

strCasePermutation :: Text
strCasePermutation = "case-permutation"

strFilterUnreachable :: Text
strFilterUnreachable = "filter-unreachable"

strOptPhaseEval :: Text
strOptPhaseEval = "opt-phase-eval"

strOptPhaseExec :: Text
strOptPhaseExec = "opt-phase-exec"

strOptPhaseGeb :: Text
strOptPhaseGeb = "opt-phase-geb"

strOptPhaseVampIR :: Text
strOptPhaseVampIR = "opt-phase-vampir"

strOptPhaseMain :: Text
strOptPhaseMain = "opt-phase-main"
