-- TODO find a better module name
module Commands.Extra.NewCompile
  ( module Commands.Extra.NewCompile,
    module Commands.Extra.Clang,
    module Juvix.Compiler.Core.Translation.FromInternal.Data.Context,
  )
where

import Commands.Base
import Commands.CompileNew.CommonOptions
import Commands.Extra.Clang
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context

getOutputFile :: (Members '[App] r) => FileExt -> Maybe (AppPath File) -> Maybe (AppPath File) -> Sem r (Path Abs File)
getOutputFile ext inp = \case
  Just out -> fromAppPathFile out
  Nothing -> do
    mainFile <- getMainFile inp
    invokeDir <- askInvokeDir
    let baseOutputFile = invokeDir <//> filename mainFile
    return (replaceExtension' (fileExtToString ext) baseOutputFile)

compileToCore :: (Members '[App, EmbedIO, TaggedLock] r) => CompileCommonOptions -> Sem r CoreResult
compileToCore opts = runPipeline (Just (opts ^. compileInputFile)) upToCore
