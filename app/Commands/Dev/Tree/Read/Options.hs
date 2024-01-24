module Commands.Dev.Tree.Read.Options where

import CommonOptions

newtype TreeReadOptions = TreeReadOptions
  { _treeReadInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''TreeReadOptions

parseTreeReadOptions :: Parser TreeReadOptions
parseTreeReadOptions = do
  _treeReadInputFile <- parseInputFile FileExtJuvixTree
  pure TreeReadOptions {..}
