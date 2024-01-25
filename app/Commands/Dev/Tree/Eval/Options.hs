module Commands.Dev.Tree.Eval.Options where

import CommonOptions

newtype TreeEvalOptions = TreeEvalOptions
  { _treeEvalInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''TreeEvalOptions

parseTreeEvalOptions :: Parser TreeEvalOptions
parseTreeEvalOptions = do
  _treeEvalInputFile <- parseInputFile FileExtJuvixTree
  pure TreeEvalOptions {..}
