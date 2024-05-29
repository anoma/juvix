module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Internal.Translation (InternalTypedResult)
import Juvix.Compiler.Internal.Translation.FromInternal.Analysis.Termination.Checker
import Juvix.Compiler.Pipeline.Loader.PathResolver
import Juvix.Compiler.Pipeline.Root
import Juvix.Compiler.Pipeline.Run
import Juvix.Data.Error qualified as Error
import Juvix.Extra.Paths.Base hiding (rootBuildDir)
import Juvix.Parser.Error
import Juvix.Prelude.Pretty hiding
  ( Doc,
  )
import System.Console.ANSI qualified as Ansi

data App :: Effect where
  ExitMsg :: ExitCode -> Text -> App m a
  ExitFailMsg :: Text -> App m a
  ExitJuvixError :: JuvixError -> App m a
  PrintJuvixError :: JuvixError -> App m ()
  FromAppFile :: AppPath File -> App m (Path Abs File)
  AskRoot :: App m Root
  AskArgs :: App m RunAppIOArgs
  AskInvokeDir :: App m (Path Abs Dir)
  AskPkgDir :: App m (Path Abs Dir)
  AskBuildDir :: App m (Path Abs Dir)
  AskPackage :: App m Package
  AskPackageGlobal :: App m Bool
  AskGlobalOptions :: App m GlobalOptions
  FromAppPathFile :: AppPath File -> App m (Path Abs File)
  GetMainAppFile :: Maybe (AppPath File) -> App m (AppPath File)
  GetMainFile :: Maybe (AppPath File) -> App m (Path Abs File)
  FromAppPathDir :: AppPath Dir -> App m (Path Abs Dir)
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
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
  (Members '[EmbedIO, TaggedLock] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem r a
runAppIO args = evalSingletonCache (readPackageRootIO root) . reAppIO args
  where
    root = args ^. runAppIOArgsRoot

reAppIO ::
  forall r a.
  (Members '[EmbedIO, TaggedLock] r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem (SCache Package ': r) a
reAppIO args@RunAppIOArgs {..} =
  interpretTop $ \case
    AskPackageGlobal -> return (_runAppIOArgsRoot ^. rootPackageType `elem` [GlobalStdlib, GlobalPackageDescription, GlobalPackageBase])
    FromAppPathFile p -> prepathToAbsFile invDir (p ^. pathPath)
    FromAppFile m -> fromAppFile' m
    GetMainAppFile m -> getMainAppFile' m
    GetMainFile m -> getMainFile' m
    FromAppPathDir p -> liftIO (prepathToAbsDir invDir (p ^. pathPath))
    RenderStdOut t
      | _runAppIOArgsGlobalOptions ^. globalOnlyErrors -> return ()
      | otherwise -> do
          sup <- liftIO (Ansi.hSupportsANSIColor stdout)
          renderIO (not (_runAppIOArgsGlobalOptions ^. globalNoColors) && sup) t
    AskGlobalOptions -> return _runAppIOArgsGlobalOptions
    AskPackage -> getPkg
    AskArgs -> return args
    AskRoot -> return _runAppIOArgsRoot
    AskInvokeDir -> return invDir
    AskPkgDir -> return (_runAppIOArgsRoot ^. rootRootDir)
    AskBuildDir -> return (resolveAbsBuildDir (_runAppIOArgsRoot ^. rootRootDir) (_runAppIOArgsRoot ^. rootBuildDir))
    Say t
      | g ^. globalOnlyErrors -> return ()
      | otherwise -> putStrLn t
    PrintJuvixError e -> printErr e
    ExitJuvixError e -> do
      printErr e
      exitFailure
    ExitMsg exitCode t -> exitMsg' (exitWith exitCode) t
    ExitFailMsg t -> exitMsg' exitFailure t
    SayRaw b -> liftIO (ByteString.putStr b)
  where
    getPkg :: (Members '[SCache Package] r') => Sem r' Package
    getPkg = cacheSingletonGet

    exitMsg' :: (Members '[EmbedIO] r') => IO x -> Text -> Sem r' x
    exitMsg' onExit t = liftIO (putStrLn t >> hFlush stdout >> onExit)

    fromAppFile' :: (Members '[EmbedIO] r') => AppPath File -> Sem r' (Path Abs File)
    fromAppFile' f = prepathToAbsFile invDir (f ^. pathPath)

    getMainFile' :: (Members '[SCache Package, EmbedIO] r') => Maybe (AppPath File) -> Sem r' (Path Abs File)
    getMainFile' = getMainAppFile' >=> fromAppFile'

    getMainAppFile' :: (Members '[SCache Package, EmbedIO] r') => Maybe (AppPath File) -> Sem r' (AppPath File)
    getMainAppFile' = \case
      Just p -> return p
      Nothing -> do
        pkg <- getPkg
        case pkg ^. packageMain of
          Just p ->
            return
              AppPath
                { _pathPath = p,
                  _pathIsInput = True
                }
          Nothing -> missingMainErr

    missingMainErr :: (Members '[EmbedIO] r') => Sem r' x
    missingMainErr =
      exitMsg'
        exitFailure
        ( "A path to the main file must be given in the CLI or specified in the `main` field of the "
            <> pack (toFilePath juvixYamlFile)
            <> " file"
        )
    invDir = _runAppIOArgsRoot ^. rootInvokeDir
    g :: GlobalOptions
    g = _runAppIOArgsGlobalOptions
    printErr e =
      hPutStrLn stderr
        . run
        . runReader (project' @GenericOptions g)
        $ Error.render (not (_runAppIOArgsGlobalOptions ^. globalNoColors)) (g ^. globalOnlyErrors) e

getEntryPoint' ::
  (Members '[App, EmbedIO, TaggedLock] r) =>
  RunAppIOArgs ->
  Maybe (AppPath File) ->
  Sem r EntryPoint
getEntryPoint' RunAppIOArgs {..} inputFile = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> liftIO getContents
        | otherwise -> return Nothing
  mainFile <- getMainAppFile inputFile
  set entryPointStdin estdin <$> entryPointFromGlobalOptionsPre root (mainFile ^. pathPath) opts

runPipelineEither ::
  (Members '[EmbedIO, TaggedLock, ProgressLog, App] r, EntryPointOptions opts) =>
  opts ->
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r (Either JuvixError (ResolverState, PipelineResult a))
runPipelineEither opts input_ p = runPipelineOptions $ do
  args <- askArgs
  entry <- applyOptions opts <$> getEntryPoint' args input_
  runIOEither entry (inject p)

getEntryPointStdin' :: (Members '[EmbedIO, TaggedLock] r) => RunAppIOArgs -> Sem r EntryPoint
getEntryPointStdin' RunAppIOArgs {..} = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoot
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> liftIO getContents
        | otherwise -> return Nothing
  set entryPointStdin estdin <$> entryPointFromGlobalOptionsNoFile root opts

fromRightGenericError :: (Members '[App] r, ToGenericError err, Typeable err) => Either err a -> Sem r a
fromRightGenericError = fromRightJuvixError . mapLeft JuvixError

fromRightJuvixError :: (Members '[App] r) => Either JuvixError a -> Sem r a
fromRightJuvixError = getRight

someBaseToAbs' :: (Members '[App] r) => SomeBase a -> Sem r (Path Abs a)
someBaseToAbs' f = do
  r <- askInvokeDir
  return (someBaseToAbs r f)

filePathToAbs :: (Members '[EmbedIO, App] r) => Prepath FileOrDir -> Sem r (Either (Path Abs File) (Path Abs Dir))
filePathToAbs fp = do
  invokeDir <- askInvokeDir
  fromPreFileOrDir invokeDir fp

askGenericOptions :: (Members '[App] r) => Sem r GenericOptions
askGenericOptions = project <$> askGlobalOptions

getEntryPoint :: (Members '[EmbedIO, App, TaggedLock] r) => Maybe (AppPath File) -> Sem r EntryPoint
getEntryPoint inputFile = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoot <- askRoot
  getEntryPoint' RunAppIOArgs {..} inputFile

getEntryPointStdin :: (Members '[EmbedIO, App, TaggedLock] r) => Sem r EntryPoint
getEntryPointStdin = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoot <- askRoot
  getEntryPointStdin' RunAppIOArgs {..}

runPipelineTermination ::
  (Members '[EmbedIO, App, TaggedLock] r) =>
  Maybe (AppPath File) ->
  Sem (Termination ': PipelineEff r) a ->
  Sem r (PipelineResult a)
runPipelineTermination input_ p = ignoreProgressLog $ do
  r <- runPipelineEither () input_ (evalTermination iniTerminationState (inject p)) >>= fromRightJuvixError
  return (snd r)

appRunProgressLog :: (Members '[EmbedIO, App] r) => Sem (ProgressLog ': r) a -> Sem r a
appRunProgressLog m = do
  g <- askGlobalOptions
  let opts =
        ProgressLogOptions
          { _progressLogOptionsUseColors = not (g ^. globalNoColors),
            _progressLogOptionsShowThreadId = g ^. globalDevShowThreadIds
          }
  if
      | g ^. globalOnlyErrors -> ignoreProgressLog m
      | otherwise -> runProgressLogIO opts m

runPipelineNoOptions ::
  (Members '[App, EmbedIO, TaggedLock] r) =>
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r a
runPipelineNoOptions = runPipeline ()

runPipelineProgress ::
  (Members '[App, EmbedIO, ProgressLog, TaggedLock] r, EntryPointOptions opts) =>
  opts ->
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r a
runPipelineProgress opts input_ p = do
  r <- runPipelineEither opts input_ (inject p) >>= fromRightJuvixError
  return (snd r ^. pipelineResult)

runPipeline ::
  (Members '[App, EmbedIO, TaggedLock] r, EntryPointOptions opts) =>
  opts ->
  Maybe (AppPath File) ->
  Sem (PipelineEff r) a ->
  Sem r a
runPipeline opts input_ =
  appRunProgressLog
    . runPipelineProgress opts input_
    . inject

runPipelineHtml ::
  (Members '[App, EmbedIO, TaggedLock] r) =>
  Bool ->
  Maybe (AppPath File) ->
  Sem r (InternalTypedResult, [InternalTypedResult])
runPipelineHtml bNonRecursive input_ =
  appRunProgressLog $
    if
        | bNonRecursive -> do
            r <- runPipelineNoOptions input_ upToInternalTyped
            return (r, [])
        | otherwise -> do
            args <- askArgs
            entry <- getEntryPoint' args input_
            runReader defaultPipelineOptions (runPipelineHtmlEither entry) >>= fromRightJuvixError

runPipelineOptions :: (Members '[App] r) => Sem (Reader PipelineOptions ': r) a -> Sem r a
runPipelineOptions m = do
  g <- askGlobalOptions
  let opt =
        defaultPipelineOptions
          { _pipelineNumThreads = g ^. globalNumThreads
          }
  runReader opt m

runPipelineEntry :: (Members '[App, ProgressLog, EmbedIO, TaggedLock] r) => EntryPoint -> Sem (PipelineEff r) a -> Sem r a
runPipelineEntry entry p = runPipelineOptions $ do
  r <- runIOEither entry (inject p) >>= fromRightJuvixError
  return (snd r ^. pipelineResult)

runPipelineSetup :: (Members '[App, EmbedIO, Reader PipelineOptions, TaggedLock] r) => Sem (PipelineEff' r) a -> Sem r a
-- runPipelineSetup p = ignoreProgressLog $ do -- TODO restore
runPipelineSetup p = appRunProgressLog $ do
  args <- askArgs
  entry <- getEntryPointStdin' args
  r <- runIOEitherPipeline entry (inject p) >>= fromRightJuvixError
  return (snd r)

newline :: (Member App r) => Sem r ()
newline = say ""

printSuccessExit :: (Member App r) => Text -> Sem r a
printSuccessExit = exitMsg ExitSuccess

getRight :: forall e a r. (Members '[App] r, AppError e) => Either e a -> Sem r a
getRight = either appError return

runAppError :: forall e r a. (AppError e, Members '[App] r) => Sem (Error e ': r) a -> Sem r a
runAppError = runErrorNoCallStackWith appError

instance AppError ParserError where
  appError = appError . JuvixError

instance AppError MegaparsecError where
  appError = appError . JuvixError

instance AppError Text where
  appError = exitFailMsg

instance AppError JuvixError where
  appError = exitJuvixError

class AppError e where
  appError :: (Members '[App] r) => e -> Sem r a
