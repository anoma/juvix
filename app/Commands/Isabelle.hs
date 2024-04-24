module Commands.Isabelle where

import Commands.Base
import Commands.Isabelle.Options
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Pretty

runCommand ::
  (Members '[EmbedIO, TaggedLock, App] r) =>
  IsabelleOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. isabelleInputFile
  res <- runPipelineNoOptions inputFile upToIsabelle
  let thy = res ^. resultTheory
  outputDir <- fromAppPathDir (opts ^. isabelleOutputDir)
  if
      | opts ^. isabelleStdout -> do
          renderStdOut (ppOutDefault thy)
          putStrLn ""
      | otherwise -> do
          ensureDir outputDir
          let file :: Path Rel File
              file =
                relFile
                  ( unpack (res ^. resultModuleId . moduleIdPath)
                      <.> isabelleFileExt
                  )
              absPath :: Path Abs File
              absPath = outputDir <//> file
          writeFileEnsureLn absPath (ppPrint thy <> "\n")
