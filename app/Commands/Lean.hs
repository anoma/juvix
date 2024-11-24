module Commands.Lean where

import Data.Text qualified as T
import Commands.Base
import Commands.Lean.Options
import Juvix.Compiler.Backend.Lean.Data.Result
import Juvix.Compiler.Backend.Lean.Pretty

runCommand ::
  (Members AppEffects r) =>
  LeanOptions ->
  Sem r ()
runCommand opts = do
  let inputFile = opts ^. leanInputFile
  res <- runPipeline opts inputFile upToLean
  let modu = res ^. resultModule
      comments = res ^. resultComments
  outputDir <- fromAppPathDir (opts ^. leanOutputDir)
  case inputFile of
    Nothing -> error "No input file provided"
    Just inp -> do
      let rawFileName = inp ^. pathPath . prepath . to T.pack
          baseFileName = 
            fromMaybe rawFileName $ 
                T.stripSuffix ".juvix" rawFileName <|> T.stripSuffix ".md" rawFileName
      if opts ^. leanStdout
        then do
          renderStdOut (ppOutDefault comments modu)
          putStrLn ""
        else do
          ensureDir outputDir
          let file :: Path Rel File
              file =
                relFile
                  ( T.unpack baseFileName
                      <.> leanFileExt
                  )
              absPath :: Path Abs File
              absPath = outputDir <//> file
          writeFileEnsureLn absPath (ppPrint comments modu <> "\n")
