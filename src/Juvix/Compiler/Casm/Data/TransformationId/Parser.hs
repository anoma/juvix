module Juvix.Compiler.Casm.Data.TransformationId.Parser (parseTransformations, TransformationId (..), completions, completionsString) where

import Juvix.Compiler.Casm.Data.TransformationId
import Juvix.Compiler.Core.Data.TransformationId.Parser.Base
import Juvix.Prelude

parseTransformations :: Text -> Either Text [TransformationId]
parseTransformations = parseTransformations' @TransformationId @PipelineId

completionsString :: String -> [String]
completionsString = completionsString' @TransformationId @PipelineId

completions :: Text -> [Text]
completions = completions' @TransformationId @PipelineId
