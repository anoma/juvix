module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Run
import Juvix.Data.Error qualified as Error
import Juvix.Extra.Paths.Base hiding (rootBuildDir)
import Juvix.Prelude.Pretty hiding
  ( Doc,
  )
import System.Console.ANSI qualified as Ansi

data App m a where
  ExitMsg :: ExitCode -> Text -> App m a
  ExitJuvixError :: JuvixError -> App m a
  PrintJuvixError :: JuvixError -> App m ()
  AskRoot :: App m Root
  AskInvokeDir :: App m (Path Abs Dir)
  AskPkgDir :: App m (Path Abs Dir)
  AskBuildDir :: App m (Path Abs Dir)
  AskPackage :: App m Package
  AskPackageGlobal :: App m Bool
  AskGlobalOptions :: App m GlobalOptions
  FromAppPathFile :: AppPath File -> App m (Path Abs File)
  GetMainFile :: Maybe (AppPath File) -> App m (Path Abs File)
  FromAppPathDir :: AppPath Dir -> App m (Path Abs Dir)
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: AppPath File -> Sem PipelineEff a -> App m (Either JuvixError (ResolverState, a))
  RunPipelineNoFileEither :: Sem PipelineEff a -> App m (Either JuvixError (ResolverState, a))
  RunCorePipelineEither :: AppPath File -> App m (Either JuvixError Artifacts)
  Say :: Text -> App m ()
  SayRaw :: ByteString -> App m ()

makeSem ''App

data RunAppIOArgs = RunAppIOArgs
  { _runAppIOArgsGlobalOptions :: GlobalOptions,
    _runAppIOArgsRoot :: Root
  }

runAppIO ::
  forall r a.
  (Members '[Embed IO, TaggedLock] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem r a
runAppIO args@RunAppIOArgs {..} =
  interpret $ \case
    AskPackageGlobal -> return (_runAppIOArgsRoot ^. rootPackageType `elem` [GlobalStdlib, GlobalPackageDescription, GlobalPackageBase])
    FromAppPathFile p -> embed (prepathToAbsFile invDir (p ^. pathPath))
    GetMainFile m -> getMainFile' m
    FromAppPathDir p -> liftIO (prepathToAbsDir invDir (p ^. pathPath))
    RenderStdOut t
      | _runAppIOArgsGlobalOptions ^. globalOnlyErrors -> return ()
      | otherwise -> embed $ do
          sup <- Ansi.hSupportsANSIColor stdout
          renderIO (not (_runAppIOArgsGlobalOptions ^. globalNoColors) && sup) t
    AskGlobalOptions -> return _runAppIOArgsGlobalOptions
    AskPackage -> getPkg
    AskRoot -> return _runAppIOArgsRoot
    AskInvokeDir -> return invDir
    AskPkgDir -> return (_runAppIOArgsRoot ^. rootRootDir)
    AskBuildDir -> return (resolveAbsBuildDir (_runAppIOArgsRoot ^. rootRootDir) (_runAppIOArgsRoot ^. rootBuildDir))
    RunCorePipelineEither input -> do
      entry <- getEntryPoint' args input
      embed (corePipelineIOEither entry)
    RunPipelineEither input p -> do
      entry <- getEntryPoint' args input
      embed (runIOEither entry p)
    RunPipelineNoFileEither p -> do
      entry <- getEntryPointStdin' args
      embed (runIOEither entry p)
    Say t
      | g ^. globalOnlyErrors -> return ()
      | otherwise -> embed (putStrLn t)
    PrintJuvixError e -> do
      printErr e
    ExitJuvixError e -> do
      printErr e
      embed exitFailure
    ExitMsg exitCode t -> exitMsg' exitCode t
    SayRaw b -> embed (ByteString.putStr b)
  where
    getPkg :: Sem r Package
    getPkg = undefined
    exitMsg' :: ExitCode -> Text -> Sem r x
    exitMsg' exitCode t = embed (putStrLn t >> hFlush stdout >> exitWith exitCode)
    getMainFile' :: Maybe (AppPath File) -> Sem r (Path Abs File)
    getMainFile' = \case
      Just p -> embed (prepathToAbsFile invDir (p ^. pathPath))
      -- Nothing -> case pkg ^. packageMain of
      Nothing -> do
        pkg <- getPkg
        case pkg ^. packageMain of
          Just p -> embed (prepathToAbsFile invDir p)
          Nothing -> missingMainErr
    missingMainErr :: Sem r x
    missingMainErr =
      exitMsg'
        (ExitFailure 1)
        ( "A path to the main file must be given in the CLI or specified in the `main` field of the "
            <> pack (toFilePath juvixYamlFile)
            <> " file"
        )
    invDir = _runAppIOArgsRoot ^. rootInvokeDir
    g :: GlobalOptions
    g = _runAppIOArgsGlobalOptions
    printErr e =
      embed $ hPutStrLn stderr $ run $ runReader (project' @GenericOptions g) $ Error.render (not (_runAppIOArgsGlobalOptions ^. globalNoColors)) (g ^. globalOnlyErrors) e

getEntryPoint' :: (Members '[Embed IO, TaggedLock] r) => RunAppIOArgs -> AppPath File -> Sem r EntryPoint
getEntryPoint' RunAppIOArgs {..} inputFile = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> liftIO getContents
        | otherwise -> return Nothing
  set entryPointStdin estdin <$> entryPointFromGlobalOptionsPre root (inputFile ^. pathPath) opts

getEntryPointStdin' :: (Members '[Embed IO, TaggedLock] r) => RunAppIOArgs -> Sem r EntryPoint
getEntryPointStdin' RunAppIOArgs {..} = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> liftIO getContents
        | otherwise -> return Nothing
  set entryPointStdin estdin <$> entryPointFromGlobalOptionsNoFile root opts

someBaseToAbs' :: (Members '[App] r) => SomeBase a -> Sem r (Path Abs a)
someBaseToAbs' f = do
  r <- askInvokeDir
  return (someBaseToAbs r f)

filePathToAbs :: (Members '[Embed IO, App] r) => Prepath FileOrDir -> Sem r (Either (Path Abs File) (Path Abs Dir))
filePathToAbs fp = do
  invokeDir <- askInvokeDir
  embed (fromPreFileOrDir invokeDir fp)

askGenericOptions :: (Members '[App] r) => Sem r GenericOptions
askGenericOptions = project <$> askGlobalOptions

getEntryPoint :: (Members '[Embed IO, App, TaggedLock] r) => AppPath File -> Sem r EntryPoint
getEntryPoint inputFile = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoot <- askRoot
  getEntryPoint' (RunAppIOArgs {..}) inputFile

runPipelineTermination :: (Member App r) => AppPath File -> Sem (Termination ': PipelineEff) a -> Sem r a
runPipelineTermination input p = do
  r <- runPipelineEither input (evalTermination iniTerminationState p)
  case r of
    Left err -> exitJuvixError err
    Right res -> return (snd res)

runPipeline :: (Member App r) => AppPath File -> Sem PipelineEff a -> Sem r a
runPipeline input p = do
  r <- runPipelineEither input p
  case r of
    Left err -> exitJuvixError err
    Right res -> return (snd res)

runPipelineNoFile :: (Member App r) => Sem PipelineEff a -> Sem r a
runPipelineNoFile p = do
  r <- runPipelineNoFileEither p
  case r of
    Left err -> exitJuvixError err
    Right res -> return (snd res)

newline :: (Member App r) => Sem r ()
newline = say ""

printSuccessExit :: (Member App r) => Text -> Sem r a
printSuccessExit = exitMsg ExitSuccess

printFailureExit :: (Member App r) => Text -> Sem r a
printFailureExit = exitMsg (ExitFailure 1)

getRight :: (Members '[App] r, AppError e) => Either e a -> Sem r a
getRight = either appError return

instance AppError Text where
  appError = printFailureExit

instance AppError JuvixError where
  appError = exitJuvixError

class AppError e where
  appError :: (Members '[App] r) => e -> Sem r a
