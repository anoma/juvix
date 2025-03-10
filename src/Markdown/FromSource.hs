-- | Import this module qualified
module Markdown.FromSource where

import Commonmark.Parser
import Juvix.Prelude
import Markdown.Language

fromFile :: (Members '[Files, Error SimpleError] r) => Path Abs File -> Sem r Block
fromFile inputFile = do
  txt <- readFile' inputFile
  case commonmark (toFilePath inputFile) txt of
    Left err -> todo
    Right block -> return block
