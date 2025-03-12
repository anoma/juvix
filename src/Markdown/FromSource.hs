-- | Import this module qualified
module Markdown.FromSource (parseText, parseFile) where

import Commonmark.Parser
import Juvix.Prelude
import Markdown.Language

parseFile :: (Members '[Files, Error SimpleError] r) => Path Abs File -> Sem r Blocks
parseFile inputFile = readFile' inputFile >>= parseText inputFile

parseText :: (Members '[Error SimpleError] r) => Path Abs File -> Text -> Sem r Blocks
parseText inputFile txt = do
  case commonmark (toFilePath inputFile) txt of
    Left err -> throw (SimpleError ("markdown parse error: " <> show err))
    Right block -> return block
