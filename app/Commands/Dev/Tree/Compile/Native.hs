module Commands.Dev.Tree.Compile.Native where

import Commands.Base
import Commands.Compile.Native.Options
import Juvix.Compiler.Backend.C qualified as C
import Juvix.Compiler.Tree.Translation.FromSource qualified as Tree

runCommand ::
  (Members '[EmbedIO, App, TaggedLock] r) =>
  NativeOptions ('InputExtension 'FileExtJuvixTree) ->
  Sem r ()
runCommand opts = do
  file <- getMainFile (opts ^. nativeCompileCommonOptions . compileInputFile)
  s <- readFile file
  tab <- getRight (mapLeft JuvixError (Tree.runParser file s))
  -- entryPoint <- getEntry pa
  entryPoint :: EntryPoint <- undefined
  C.MiniCResult {..} <-
    getRight
      . run
      . runReader entryPoint
      . runError @JuvixError
      $ treeToMiniC tab
  let inputCFile = undefined
  cFile <- inputCFile file
  writeFileEnsureLn cFile _resultCCode
  undefined

-- outfile <- Compile.outputFile _pipelineArgOptions
-- Compile.runCommand
--   _pipelineArgOptions
--     { _compileInputFile = AppPath (preFileFromAbs cFile) False,
--       _compileOutputFile = Just (AppPath (preFileFromAbs outfile) False)
--     }
-- where
--   inputCFile :: Path Abs File -> Sem r (Path Abs File)
--   inputCFile inputFileCompile = do
--     buildDir <- askBuildDir
--     ensureDir buildDir
--     return (buildDir <//> replaceExtension' ".c" (filename inputFileCompile))
