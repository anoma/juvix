{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Compiler.Core.Data.TransformationId.Parser.Base (parseTransformations', completions', completionsString') where

import Data.Text qualified as Text
import Juvix.Compiler.Core.Data.TransformationId.Base
import Juvix.Prelude
import Juvix.Prelude.Parsing (MonadParsec)
import Juvix.Prelude.Parsing qualified as P
import Text.Megaparsec.Char.Lexer qualified as L

parseTransformations' :: forall t p. (PipelineId' t p) => Text -> Either Text [t]
parseTransformations' = fmap (fromTransformationLikes @t @p) . P.parseHelper transformations

completionsString' :: forall t p. (PipelineId' t p) => String -> [String]
completionsString' = map unpack . completions' @t @p . pack

completions' :: forall t p. (PipelineId' t p) => Text -> [Text]
completions' = fromRight [] . P.parseHelper (pcompletions @t @p)

transformations :: (PipelineId' t p, MonadParsec e Text m) => m [TransformationLikeId' t p]
transformations = do
  P.hspace
  P.sepEndBy transformationLike comma <* P.eof

-- | returns a possible list of completions
pcompletions :: forall t p e m. (PipelineId' t p, MonadParsec e Text m) => m [Text]
pcompletions = do
  P.hspace
  l <- P.sepEndBy transformationLike comma
  rest <- Text.strip <$> P.takeRest
  return [ppTransL (notNull l) l <> str | str <- allStrings @t @p, Text.isPrefixOf rest str]
  where
    ppTransL :: Bool -> [TransformationLikeId' t p] -> Text
    ppTransL c =
      let f :: Text -> Text = if c then (<> ",") else id
       in f . Text.intercalate "," . map transformationLikeText

lexeme :: (MonadParsec e Text m) => m a -> m a
lexeme = L.lexeme P.hspace

comma :: (MonadParsec e Text m) => m ()
comma = symbol ","

symbol :: (MonadParsec e Text m) => Text -> m ()
symbol = void . lexeme . P.chunk

transformationLike :: (PipelineId' t p, MonadParsec e Text m) => m (TransformationLikeId' t p)
transformationLike =
  TransformationId
    <$> parseTransformation
    <|> PipelineId
    <$> parsePipeline

transformationLikeText :: (PipelineId' t p) => TransformationLikeId' t p -> Text
transformationLikeText = \case
  TransformationId t -> transformationText t
  PipelineId p -> pipelineText p

parsePipeline :: (PipelineId' t p, MonadParsec e Text m) => m p
parsePipeline = P.choice [symbol (pipelineText t) $> t | t <- allElements]

parseTransformation :: (Enum t, Bounded t, TransformationId' t, MonadParsec e Text m) => m t
parseTransformation = P.choice [symbol (transformationText t) $> t | t <- allElements]

allStrings :: forall t p. (PipelineId' t p) => [Text]
allStrings = map (transformationLikeText @t @p) (allTransformationLikeIds @t @p)
