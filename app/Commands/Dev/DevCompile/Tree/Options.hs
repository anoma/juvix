module Commands.Dev.DevCompile.Tree.Options where

import Commands.CompileNew.CommonOptions
import CommonOptions

data TreeOptions = TreeOptions
  { _treeCompileCommonOptions :: CompileCommonOptions
  }
  deriving stock (Data)

parseTree :: Parser TreeOptions
parseTree = do
  _treeCompileCommonOptions <- parseCompileCommonOptions
  pure TreeOptions {..}
