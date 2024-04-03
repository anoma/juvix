module Commands.Dev.DevCompile.Tree.Options
  ( module Commands.Dev.DevCompile.Tree.Options,
    module Commands.CompileNew.CommonOptions,
  )
where

import Commands.CompileNew.CommonOptions
import CommonOptions

data TreeOptions = TreeOptions
  { _treeCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''TreeOptions

parseTree :: Parser TreeOptions
parseTree = do
  _treeCompileCommonOptions <- parseCompileCommonOptionsMain
  pure TreeOptions {..}