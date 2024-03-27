-- TODO find a better module name
module Commands.Extra.NewCompile
  ( module Commands.Extra.NewCompile,
    module Commands.Extra.Clang,
  )
where

import Commands.Base
import Commands.CompileNew.CommonOptions
import Commands.Extra.Clang
import Juvix.Compiler.Core qualified as Core

getOutputFile :: (Members '[App] r) => FileExt -> Maybe (AppPath File) -> Maybe (AppPath File) -> Sem r (Path Abs File)
getOutputFile ext inp mout = do
  case mout of
    Just out -> fromAppPathFile out
    Nothing -> do
      mainFile <- getMainFile inp
      return (replaceExtension' (unpack (fileExtToText ext)) mainFile)

compileToCore :: (Members '[App, EmbedIO, TaggedLock] r) => CompileCommonOptions -> Sem r Core.CoreResult
compileToCore opts = do
  inputFile <- getMainFile (opts ^. compileInputFile)
  runPipeline (AppPath (preFileFromAbs inputFile) True) upToCore
