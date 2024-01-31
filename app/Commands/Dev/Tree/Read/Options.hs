module Commands.Dev.Tree.Read.Options where

import CommonOptions
import Juvix.Compiler.Tree.Data.TransformationId

data TreeReadOptions = TreeReadOptions
  { _treeReadTransformations :: [TransformationId],
    _treeReadEval :: Bool,
    _treeReadNoPrint :: Bool,
    _treeReadInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''TreeReadOptions

parseTreeReadOptions :: Parser TreeReadOptions
parseTreeReadOptions = do
  _treeReadNoPrint <-
    switch
      ( long "no-print"
          <> help "Do not print the transformed code"
      )
  _treeReadEval <-
    switch
      ( long "eval"
          <> help "Evaluate after the transformation"
      )
  _treeReadTransformations <- optTreeTransformationIds
  _treeReadInputFile <- parseInputFile FileExtJuvixTree
  pure TreeReadOptions {..}
