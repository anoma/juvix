module Commands.Isabelle where

import Commands.Base
import Commands.Isabelle.Options
import Juvix.Compiler.Backend.Isabelle.Data.Result
import Juvix.Compiler.Backend.Isabelle.Language
import Juvix.Compiler.Backend.Isabelle.Pretty

runCommand ::
  (Members AppEffects r) =>
  IsabelleOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. isabelleInputFile
  res <- runPipeline opts inputFile upToIsabelle
  let thy = res ^. resultTheory
      comments = res ^. resultComments
  outputDir <- fromAppPathDir (opts ^. isabelleOutputDir)
  if
      | opts ^. isabelleStdout -> do
          renderStdOut (ppOutDefault comments thy)
          putStrLn ""
      | otherwise -> do
          ensureDir outputDir
          let file :: Path Rel File
              file =
                relFile
                  ( unpack (thy ^. theoryName . namePretty)
                      <.> isabelleFileExt
                  )
              absPath :: Path Abs File
              absPath = outputDir <//> file
          writeFileEnsureLn absPath (ppPrint comments thy <> "\n")
