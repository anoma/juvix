-- TODO find a better module name
module Commands.Extra.NewCompile
  ( module Commands.Extra.NewCompile,
    module Commands.Extra.Clang,
    module Juvix.Compiler.Core.Translation.FromInternal.Data.Context,
    module Commands.Extra.Compile.Options,
  )
where

import Commands.Base
import Commands.Extra.Clang
import Commands.Extra.Compile.Options (CompileTarget (..), compileTargetDescription)
import Juvix.Compiler.Core.Translation.FromInternal.Data.Context

getOutputFile ::
  (Members '[App] r) =>
  FileExt ->
  Maybe (AppPath File) ->
  Maybe (AppPath File) ->
  Sem r (Path Abs File)
getOutputFile ext inp = \case
  Just out -> fromAppPathFile out
  Nothing -> do
    mainFile <- getMainFile inp
    invokeDir <- askInvokeDir
    let baseOutputFile = invokeDir <//> filename mainFile
    return (replaceExtension' (fileExtToString ext) baseOutputFile)

getOutputDir ::
  (Members '[App] r) =>
  FileExt ->
  Maybe (AppPath File) ->
  Maybe (AppPath Dir) ->
  Sem r (Path Abs Dir)
getOutputDir ext inp = \case
  Just out -> fromAppPathDir out
  Nothing -> do
    mainFile <- getMainFile inp
    invokeDir <- askInvokeDir
    let baseOutputDir = invokeDir <//> filename (replaceExtension' (fileExtToString ext) mainFile)
    return $ pathFileToPathDir baseOutputDir

commandTargetHelper :: CompileTarget -> Parser a -> Mod CommandFields a
commandTargetHelper t parseCommand =
  let cmd = show t
      descr = compileTargetDescription t
   in command cmd (info parseCommand (progDesc descr))

commandTargetsHelper :: [(CompileTarget, Parser a)] -> Parser a
commandTargetsHelper supportedTargets =
  hsubparser $
    mconcat
      [ commandTargetHelper backend parser
        | (backend, parser) <- supportedTargets
      ]
