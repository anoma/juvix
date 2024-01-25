module Commands.Dev.Tree.FromAsm.Options where

import CommonOptions

newtype TreeFromAsmOptions = TreeFromAsmOptions
  { _treeFromAsmInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''TreeFromAsmOptions

parseTreeFromAsmOptions :: Parser TreeFromAsmOptions
parseTreeFromAsmOptions = do
  _treeFromAsmInputFile <- parseInputFile FileExtJuvixAsm
  pure TreeFromAsmOptions {..}
