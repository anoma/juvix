module Commands.Dev.Reg.Read.Options where

import CommonOptions
import Juvix.Compiler.Reg.Data.TransformationId

data RegReadOptions = RegReadOptions
  { _regReadTransformations :: [TransformationId],
    _regReadRun :: Bool,
    _regReadNoPrint :: Bool,
    _regReadInputFile :: AppPath File
  }
  deriving stock (Data)

makeLenses ''RegReadOptions

parseRegReadOptions :: Parser RegReadOptions
parseRegReadOptions = do
  _regReadNoPrint <- optReadNoPrint
  _regReadRun <- optReadRun
  _regReadTransformations <- optRegTransformationIds
  _regReadInputFile <- parseInputFile FileExtJuvixReg
  pure RegReadOptions {..}
