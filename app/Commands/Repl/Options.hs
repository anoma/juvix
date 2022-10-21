module Commands.Repl.Options where

import CommonOptions

newtype ReplOptions = ReplOptions
  { _replInputFile :: Maybe Path }
  deriving stock (Data)

makeLenses ''ReplOptions

parseRepl :: Parser ReplOptions
parseRepl = do
  _replInputFile <- optional parseInputJuvixFile
  pure ReplOptions {..}
