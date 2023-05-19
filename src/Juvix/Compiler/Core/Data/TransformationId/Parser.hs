module Juvix.Compiler.Core.Data.TransformationId.Parser (parseTransformations, TransformationId (..), completions, completionsString) where

import Data.Text qualified as Text
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Prelude
import Juvix.Prelude.Parsing (MonadParsec)
import Juvix.Prelude.Parsing qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

parseTransformations :: Text -> Either Text [TransformationId]
parseTransformations = fmap fromTransformationLikes . P.parseHelper transformations

completionsString :: String -> [String]
completionsString = map unpack . completions . pack

completions :: Text -> [Text]
completions = fromRight [] . P.parseHelper pcompletions

transformations :: (MonadParsec e Text m) => m [TransformationLikeId]
transformations = do
  P.hspace
  P.sepEndBy transformationLike comma <* P.eof

-- | returns a possible list of completions
pcompletions :: (MonadParsec e Text m) => m [Text]
pcompletions = do
  P.hspace
  l <- P.sepEndBy transformationLike comma
  rest <- Text.strip <$> P.takeRest
  return [ppTransL (notNull l) l <> str | str <- allStrings, Text.isPrefixOf rest str]
  where
    ppTransL :: Bool -> [TransformationLikeId] -> Text
    ppTransL c =
      let f :: Text -> Text = if c then (<> ",") else id
       in f . Text.intercalate "," . map transformationLikeText

lexeme :: (MonadParsec e Text m) => m a -> m a
lexeme = L.lexeme P.hspace

comma :: (MonadParsec e Text m) => m ()
comma = symbol ","

symbol :: (MonadParsec e Text m) => Text -> m ()
symbol = void . lexeme . P.chunk

transformationLike :: MonadParsec e Text m => m TransformationLikeId
transformationLike =
  TransformationId <$> transformation
    <|> PipelineId <$> parsePipeline

pipelineText :: PipelineId -> Text
pipelineText = \case
  PipelineEval -> strEvalPipeline
  PipelineNormalize -> strNormalizePipeline
  PipelineGeb -> strGebPipeline
  PipelineVampIR -> strVampIRPipeline
  PipelineStripped -> strStrippedPipeline

transformationLikeText :: TransformationLikeId -> Text
transformationLikeText = \case
  TransformationId t -> transformationText t
  PipelineId p -> pipelineText p

transformationText :: TransformationId -> Text
transformationText = \case
  LambdaLetRecLifting -> strLifting
  LetRecLifting -> strLetRecLifting
  TopEtaExpand -> strTopEtaExpand
  MatchToCase -> strMatchToCase
  NaiveMatchToCase -> strNaiveMatchToCase
  EtaExpandApps -> strEtaExpandApps
  Identity -> strIdentity
  RemoveTypeArgs -> strRemoveTypeArgs
  MoveApps -> strMoveApps
  NatToPrimInt -> strNatToPrimInt
  IntToPrimInt -> strIntToPrimInt
  ConvertBuiltinTypes -> strConvertBuiltinTypes
  ComputeTypeInfo -> strComputeTypeInfo
  UnrollRecursion -> strUnrollRecursion
  DisambiguateNames -> strDisambiguateNames
  CheckGeb -> strCheckGeb
  CheckExec -> strCheckExec
  Normalize -> strNormalize
  LetFolding -> strLetFolding
  LambdaFolding -> strLambdaFolding
  LetHoisting -> strLetHoisting
  Inlining -> strInlining
  FoldTypeSynonyms -> strFoldTypeSynonyms
  OptPhaseEval -> strOptPhaseEval
  OptPhaseExec -> strOptPhaseExec
  OptPhaseGeb -> strOptPhaseGeb
  OptPhaseMain -> strOptPhaseMain

parsePipeline :: MonadParsec e Text m => m PipelineId
parsePipeline = P.choice [symbol (pipelineText t) $> t | t <- allElements]

transformation :: MonadParsec e Text m => m TransformationId
transformation = P.choice [symbol (transformationText t) $> t | t <- allElements]

allStrings :: [Text]
allStrings = map transformationLikeText allTransformationLikeIds

strLetHoisting :: Text
strLetHoisting = "let-hoisting"

strEvalPipeline :: Text
strEvalPipeline = "pipeline-eval"

strNormalizePipeline :: Text
strNormalizePipeline = "pipeline-normalize"

strGebPipeline :: Text
strGebPipeline = "pipeline-geb"

strVampIRPipeline :: Text
strVampIRPipeline = "pipeline-vampir"

strStrippedPipeline :: Text
strStrippedPipeline = "pipeline-stripped"

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

strCheckGeb :: Text
strCheckGeb = "check-geb"

strCheckExec :: Text
strCheckExec = "check-exec"

strNormalize :: Text
strNormalize = "normalize"

strLetFolding :: Text
strLetFolding = "let-folding"

strLambdaFolding :: Text
strLambdaFolding = "lambda-folding"

strInlining :: Text
strInlining = "inlining"

strFoldTypeSynonyms :: Text
strFoldTypeSynonyms = "fold-type-synonyms"

strOptPhaseEval :: Text
strOptPhaseEval = "opt-phase-eval"

strOptPhaseExec :: Text
strOptPhaseExec = "opt-phase-exec"

strOptPhaseGeb :: Text
strOptPhaseGeb = "opt-phase-geb"

strOptPhaseMain :: Text
strOptPhaseMain = "opt-phase-main"
