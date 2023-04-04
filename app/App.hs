module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Pipeline
import Juvix.Data.Error qualified as Error
import Juvix.Prelude.Pretty hiding (Doc)
import System.Console.ANSI qualified as Ansi
import System.Directory qualified as D

data App m a where
  ExitMsg :: ExitCode -> Text -> App m a
  ExitJuvixError :: JuvixError -> App m a
  PrintJuvixError :: JuvixError -> App m ()
  AskRoots :: App m Roots
  AskInvokeDir :: App m (Path Abs Dir)
  AskPkgDir :: App m (Path Abs Dir)
  AskBuildDir :: App m (Path Abs Dir)
  AskPackage :: App m Package
  AskPackageGlobal :: App m Bool
  AskGlobalOptions :: App m GlobalOptions
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: AppPath File -> Sem PipelineEff a -> App m (Either JuvixError (ResolverState, a))
  RunCorePipelineEither :: AppPath File -> App m (Either JuvixError Artifacts)
  Say :: Text -> App m ()
  SayRaw :: ByteString -> App m ()

makeSem ''App

data RunAppIOArgs = RunAppIOArgs
  { _runAppIOArgsGlobalOptions :: GlobalOptions,
    _runAppIOArgsRoots :: Roots
  }

runAppIO ::
  forall r a.
  (Member (Embed IO) r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem r a
runAppIO args@RunAppIOArgs {..} =
  interpret $ \case
    AskPackageGlobal -> return (_runAppIOArgsRoots ^. rootsPackageGlobal)
    RenderStdOut t
      | _runAppIOArgsGlobalOptions ^. globalOnlyErrors -> return ()
      | otherwise -> embed $ do
          sup <- Ansi.hSupportsANSIColor stdout
          renderIO (not (_runAppIOArgsGlobalOptions ^. globalNoColors) && sup) t
    AskGlobalOptions -> return _runAppIOArgsGlobalOptions
    AskPackage -> return (_runAppIOArgsRoots ^. rootsPackage)
    AskRoots -> return _runAppIOArgsRoots
    AskInvokeDir -> return (_runAppIOArgsRoots ^. rootsInvokeDir)
    AskPkgDir -> return (_runAppIOArgsRoots ^. rootsRootDir)
    AskBuildDir -> return (_runAppIOArgsRoots ^. rootsBuildDir)
    RunCorePipelineEither input -> do
      entry <- embed (getEntryPoint' args input)
      embed (corePipelineIOEither entry)
    RunPipelineEither input p -> do
      entry <- embed (getEntryPoint' args input)
      embed (runIOEither entry p)
    Say t
      | g ^. globalOnlyErrors -> return ()
      | otherwise -> embed (putStrLn t)
    PrintJuvixError e -> do
      printErr e
    ExitJuvixError e -> do
      printErr e
      embed exitFailure
    ExitMsg exitCode t -> embed (putStrLn t >> hFlush stdout >> exitWith exitCode)
    SayRaw b -> embed (ByteString.putStr b)
  where
    g :: GlobalOptions
    g = _runAppIOArgsGlobalOptions
    printErr e =
      embed $ hPutStrLn stderr $ run $ runReader (project' @GenericOptions g) $ Error.render (not (_runAppIOArgsGlobalOptions ^. globalNoColors)) (g ^. globalOnlyErrors) e

getEntryPoint' :: RunAppIOArgs -> AppPath File -> IO EntryPoint
getEntryPoint' RunAppIOArgs {..} inputFile = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsRoots ^. rootsRootDir
      pkg = _runAppIOArgsRoots ^. rootsPackage
      isGlobal = _runAppIOArgsRoots ^. rootsPackageGlobal
      invokeDir = _runAppIOArgsRoots ^. rootsInvokeDir
      buildDir = _runAppIOArgsRoots ^. rootsBuildDir
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> getContents
        | otherwise -> return Nothing
  return $
    (entryPointFromGlobalOptions (pkg, isGlobal) root (someBaseToAbs invokeDir (inputFile ^. pathPath)) opts)
      { _entryPointBuildDir = Abs buildDir,
        _entryPointPackage = pkg,
        _entryPointStdin = estdin
      }

someBaseToAbs' :: (Members '[App] r) => SomeBase a -> Sem r (Path Abs a)
someBaseToAbs' f = do
  r <- askInvokeDir
  return (someBaseToAbs r f)

filePathToAbs :: Members '[Embed IO, App] r => FilePath -> Sem r (Either (Path Abs File) (Path Abs Dir))
filePathToAbs fp = do
  invokeDir <- askInvokeDir
  absPath <- embed (D.canonicalizePath (toFilePath invokeDir </> fp))
  isDirectory <- embed (D.doesDirectoryExist absPath)
  if
      | isDirectory -> Right <$> embed @IO (parseAbsDir absPath)
      | otherwise -> Left <$> embed @IO (parseAbsFile absPath)

askGenericOptions :: (Members '[App] r) => Sem r GenericOptions
askGenericOptions = project <$> askGlobalOptions

getEntryPoint :: (Members '[Embed IO, App] r) => AppPath File -> Sem r EntryPoint
getEntryPoint inputFile = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsRoots <- askRoots
  embed (getEntryPoint' (RunAppIOArgs {..}) inputFile)

runPipeline :: (Member App r) => AppPath File -> Sem PipelineEff a -> Sem r a
runPipeline input p = do
  r <- runPipelineEither input p
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
