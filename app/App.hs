module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Package
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
  AskArgs :: App m RunAppIOArgs
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
  RunCorePipelineEither :: AppPath File -> App m (Either JuvixError Artifacts)
  Say :: Text -> App m ()
  SayRaw :: ByteString -> App m ()

data RunAppIOArgs = RunAppIOArgs
  { _runAppIOArgsGlobalOptions :: GlobalOptions,
    _runAppIOArgsRoot :: Root
  }

makeSem ''App
makeLenses ''RunAppIOArgs

runAppIO ::
  forall r a.
  (Members '[Embed IO, TaggedLock] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem r a
runAppIO args = evalSingletonCache (readPackageRootIO root) . reAppIO args
  where
    root = args ^. runAppIOArgsRoot

reAppIO ::
  forall r a.
  (Members '[Embed IO, TaggedLock] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem (SCache Package ': r) a
reAppIO args@RunAppIOArgs {..} =
  reinterpret $ \case
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
    AskArgs -> return args
    AskRoot -> return _runAppIOArgsRoot
    AskInvokeDir -> return invDir
    AskPkgDir -> return (_runAppIOArgsRoot ^. rootRootDir)
    AskBuildDir -> return (resolveAbsBuildDir (_runAppIOArgsRoot ^. rootRootDir) (_runAppIOArgsRoot ^. rootBuildDir))
    RunCorePipelineEither input -> do
      entry <- getEntryPoint' args input
      embed (corePipelineIOEither entry)
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
    getPkg :: (Members '[SCache Package] r') => Sem r' Package
    getPkg = cacheSingletonGet

    exitMsg' :: (Members '[Embed IO] r') => ExitCode -> Text -> Sem r' x
    exitMsg' exitCode t = liftIO (putStrLn t >> hFlush stdout >> exitWith exitCode)

    getMainFile' :: (Members '[SCache Package, Embed IO] r') => Maybe (AppPath File) -> Sem r' (Path Abs File)
    getMainFile' = \case
      Just p -> embed (prepathToAbsFile invDir (p ^. pathPath))
      -- Nothing -> case pkg ^. packageMain of
      Nothing -> do
        pkg <- getPkg
        case pkg ^. packageMain of
          Just p -> embed (prepathToAbsFile invDir p)
          Nothing -> missingMainErr

    missingMainErr :: (Members '[Embed IO] r') => Sem r' x
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

runPipelineNoFileEither :: (Members '[Embed IO, TaggedLock, App] r) => Sem (PipelineEff r) a -> Sem r (Either JuvixError (ResolverState, a))
runPipelineNoFileEither p = do
  args <- askArgs
  entry <- getEntryPointStdin' args
  snd <$> runIOEither entry p

runPipelineEither :: (Members '[Embed IO, TaggedLock, App] r) => AppPath File -> Sem (PipelineEff r) a -> Sem r (Either JuvixError (ResolverState, a))
runPipelineEither input p = do
  args <- askArgs
  entry <- getEntryPoint' args input
  snd <$> runIOEither entry p

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

runPipelineTermination :: (Members '[App, Embed IO, TaggedLock] r) => AppPath File -> Sem (Termination ': PipelineEff r) a -> Sem r a
runPipelineTermination input p = do
  r <- runPipelineEither input (evalTermination iniTerminationState p)
  case r of
    Left err -> exitJuvixError err
    Right res -> return (snd res)

runPipeline :: (Members '[App, Embed IO, TaggedLock] r) => AppPath File -> Sem (PipelineEff r) a -> Sem r a
runPipeline input p = do
  r <- runPipelineEither input p
  case r of
    Left err -> exitJuvixError err
    Right res -> return (snd res)

runPipelineNoFile :: (Members '[App, Embed IO, TaggedLock] r) => Sem (PipelineEff r) a -> Sem r a
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
