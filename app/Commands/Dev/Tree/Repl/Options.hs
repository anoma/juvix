module Commands.Dev.Tree.Repl.Options where

import CommonOptions

data TreeReplOptions = TreeReplOptions
  deriving stock (Data)

makeLenses ''TreeReplOptions

parseTreeReplOptions :: Parser TreeReplOptions
parseTreeReplOptions = do
  pure TreeReplOptions
