module Juvix.Compiler.Core.Data.TransformationId.Parser (parseTransformations, TransformationId (..), completions, completionsString) where

import Data.Text qualified as Text
import Juvix.Compiler.Core.Data.TransformationId
import Juvix.Prelude
import Juvix.Prelude.Pretty hiding (comma)
import Text.Megaparsec as P
import Text.Megaparsec.Char qualified as L
import Text.Megaparsec.Char.Lexer qualified as L

parseHelper :: Parsec Void Text a -> Text -> Either Text a
parseHelper p t = case runParser p "<input>" t of
  Left (err :: ParseErrorBundle Text Void) -> Left (prettyText (errorBundlePretty err))
  Right r -> return r

parseTransformations :: Text -> Either Text [TransformationId]
parseTransformations = fmap fromTransformationLikes . parseHelper transformations

completionsString :: String -> [String]
completionsString = map unpack . completions . pack

completions :: Text -> [Text]
completions = fromRight [] . parseHelper pcompletions

transformations :: (MonadParsec e Text m) => m [TransformationLikeId]
transformations = do
  L.hspace
  sepEndBy transformationLike comma <* eof

-- | returns a possible list of completions
pcompletions :: (MonadParsec e Text m) => m [Text]
pcompletions = do
  L.hspace
  l <- sepEndBy transformationLike comma
  rest <- Text.strip <$> takeRest
  return [ppTransL (notNull l) l <> str | str <- allStrings, Text.isPrefixOf rest str]
  where
    ppTransL :: Bool -> [TransformationLikeId] -> Text
    ppTransL c =
      let f :: Text -> Text = if c then (<> ",") else id
       in f . Text.intercalate "," . map transformationLikeText

lexeme :: (MonadParsec e Text m) => m a -> m a
lexeme = L.lexeme L.hspace

comma :: (MonadParsec e Text m) => m ()
comma = symbol ","

symbol :: (MonadParsec e Text m) => Text -> m ()
symbol = void . lexeme . chunk

transformationLike :: MonadParsec e Text m => m TransformationLikeId
transformationLike =
  TransformationId <$> transformation
    <|> PipelineId <$> parsePipeline

pipelineText :: PipelineId -> Text
pipelineText = \case
  PipelineEval -> strEvalPipeline
  PipelineGeb -> strGebPipeline
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
  LetFolding -> strLetFolding
  FoldTypeSynonyms -> strFoldTypeSynonyms

parsePipeline :: MonadParsec e Text m => m PipelineId
parsePipeline = choice [symbol (pipelineText t) $> t | t <- allElements]

transformation :: MonadParsec e Text m => m TransformationId
transformation = choice [symbol (transformationText t) $> t | t <- allElements]

allStrings :: [Text]
allStrings = map transformationLikeText allTransformationLikeIds

strEvalPipeline :: Text
strEvalPipeline = "pipeline-eval"

strGebPipeline :: Text
strGebPipeline = "pipeline-geb"

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

strLetFolding :: Text
strLetFolding = "let-folding"

strFoldTypeSynonyms :: Text
strFoldTypeSynonyms = "fold-type-synonyms"
