module Commands.Dev.DevCompile.Tree.Options
  ( module Commands.Dev.DevCompile.Tree.Options,
    module Commands.Compile.CommonOptions,
  )
where

import Commands.Compile.CommonOptions
import CommonOptions

data TreeOptions = TreeOptions
  { _treeCompileCommonOptions :: CompileCommonOptionsMain
  }
  deriving stock (Data)

makeLenses ''TreeOptions

parseTree :: Parser TreeOptions
parseTree = do
  _treeCompileCommonOptions <- parseCompileCommonOptionsJuvixMain
  pure TreeOptions {..}
