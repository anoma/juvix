module Commands.Lean where

import Commands.Base
import Commands.Lean.Options
import Juvix.Compiler.Backend.Lean.Data.Result
import Juvix.Compiler.Backend.Lean.Language
import Juvix.Compiler.Backend.Lean.Pretty

runCommand ::
  (Members AppEffects r) =>
  LeanOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. leanInputFile
  res <- runPipeline opts inputFile upToLean
  let modu = res ^. resultModule -- Lean module result
      comments = res ^. resultComments
  outputDir <- fromAppPathDir (opts ^. leanOutputDir)
  if
      | opts ^. leanStdout -> do
          renderStdOut (ppOutDefault comments modu)
          putStrLn ""
      | otherwise -> do
          ensureDir outputDir
          let file :: Path Rel File
              file =
                relFile
                  ( unpack (modu ^. moduleName . namePretty)
                      <.> leanFileExt
                  )
              absPath :: Path Abs File
              absPath = outputDir <//> file
          writeFileEnsureLn absPath (ppPrint comments modu <> "\n")
