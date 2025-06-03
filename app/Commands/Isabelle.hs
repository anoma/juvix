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
  (r, rs) <- runPipelineUpTo (opts ^. isabelleNonRecursive) opts inputFile upToIsabelle
  let pkg = r ^. resultModuleId . moduleIdPackageId
  traceM ("translated package: " <> show (pkg ^. packageIdName))
  mapM_ (translateTyped opts pkg) (r : rs)

translateTyped :: (Members AppEffects r) => IsabelleOptions -> PackageId -> Result -> Sem r ()
translateTyped opts pkg res
  | res ^. resultModuleId . moduleIdPackageId . packageIdName == pkg ^. packageIdName = do
      traceM ("translateTyped module: " <> prettyText (res ^. resultModuleId . moduleIdPath))
      traceM ("translateTyped package: " <> show (res ^. resultModuleId . moduleIdPackageId . packageIdName))
      let thy = res ^. resultTheory
          comments = res ^. resultComments
      outputDir <- fromAppPathDir (opts ^. isabelleOutputDir)
      if
          | opts ^. isabelleStdout ->
              renderStdOutLn (ppOutDefault comments thy)
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
  | otherwise = do
      traceM ("translateTyped module: " <> prettyText (res ^. resultModuleId . moduleIdPath))
      traceM ("Skipping translation for package: " <> show (res ^. resultModuleId . moduleIdPackageId . packageIdName))
      return ()
