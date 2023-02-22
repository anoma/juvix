{-# LANGUAGE QuasiQuotes #-}
module Commands.Format where

import Commands.Base
import Commands.Format.Options
import Data.Text qualified as T
import Juvix.Compiler.Concrete.Language
import Juvix.Compiler.Concrete.Print (ppOutDefault)
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.Scoping qualified as Scoper
import Juvix.Prelude.Pretty (toPlainText)
import Data.String.Interpolate (i)

runCommand :: forall r. Members '[Embed IO, App] r => FormatOptions -> Sem r ()
runCommand opts = mapM checkFile (filesToFormat opts)
  -- res :: Scoper.ScoperResult <- runPipeline (opts ^. formatInputFile) upToScoping
  -- let modules :: NonEmpty (Module 'Scoped 'ModuleTop) = res ^. Scoper.resultModules
  --     formattedFile :: Text = formatFile (res ^. Scoper.comments) modules
  -- inputFile' :: Path Abs File <- inputFile
  -- fileContents <- embed (readFile (toFilePath (inputFile')))
  -- unless (fileContents == formattedFile) exitError
  -- where
  --   exitError :: Sem r ()
  --   exitError = do
  --     inputFile' <- toFilePath <$> inputFile
  --     exitMsg (ExitFailure 1) [i|File #{inputFile'} is not formatted|]

  --   inputFile :: Sem r (Path Abs File)
  --   inputFile = do
  --     invokeDir <- askInvokeDir
  --     return (someBaseToAbs invokeDir (opts ^. formatInputFile . pathPath))

filesToFormat :: forall r. Member App r => FormatOptions -> Sem r [Path Abs File]
filesToFormat opts = case opts ^. formatInputFile of
  Just fp -> return <$> inputFile fp
  Nothing -> pkgFiles
  where
    inputFile :: AppPath File -> Sem r (Path Abs File)
    inputFile appFile = do
      invokeDir <- askInvokeDir
      return (someBaseToAbs invokeDir (appFile ^. pathPath))

    pkgFiles :: Sem r [Path Abs File]
    pkgFiles = undefined

checkFile' :: forall r. Members '[Embed IO, App] r => Path Abs File -> Sem r ()
checkFile' fp = do
  let appFile = AppPath fp True
  undefined

checkFile :: forall r. Members '[Embed IO, App] r => AppPath File -> Sem r ()
checkFile appFile = do
  inputFilePath <- inputFile
  res :: Scoper.ScoperResult <- runPipeline appFile upToScoping
  let modules :: NonEmpty (Module 'Scoped 'ModuleTop) = res ^. Scoper.resultModules
      formattedFile :: Text = formatFile (res ^. Scoper.comments) modules
      fp :: FilePath = toFilePath inputFilePath
  fileContents <- embed (readFile (toFilePath inputFilePath))
  unless (fileContents == formattedFile) (exitError fp)
  where
    exitError :: FilePath -> Sem r ()
    exitError fp = exitMsg (ExitFailure 1) [i|File #{fp} is not formatted|]

    inputFile :: Sem r (Path Abs File)
    inputFile = do
      invokeDir <- askInvokeDir
      return (someBaseToAbs invokeDir (appFile ^. pathPath))




formatModule :: Comments -> Module 'Scoped 'ModuleTop -> Text
formatModule cs m = toPlainText (ppOutDefault cs m)

formatFile :: Comments -> NonEmpty (Module 'Scoped 'ModuleTop) -> Text
formatFile cs = T.concat . toList . fmap (formatModule cs)

showDiff :: forall r. Members '[Embed IO, App] r => Text -> Text -> Sem r ()
showDiff t1 t2 = do
  hasCommand <- checkCommand "diff"
  unless (isNothing hasCommand) go
  where
    go :: Sem r ()
    go = writeFiles $(mkAbsDir "/tmp")

    writeFiles :: Path Abs Dir -> Sem r ()
    writeFiles tmpDir = do
      let f1 = tmpDir <//> relFile "f1"
      let f2 = tmpDir <//> relFile "f2"
      embed (putStrLn (pack (toFilePath f1)))
      embed (writeFile (toFilePath f1) t1)
      embed (writeFile (toFilePath f2) t2)
      res <- runExternalCommand "diff" [pack (toFilePath f1), pack (toFilePath f2)]
      embed $ putStrLn (res ^. runCmdResStdout)
