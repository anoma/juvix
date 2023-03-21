module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Concrete.Translation.FromParsed.Analysis.PathResolver
import Juvix.Compiler.Core qualified as Core
import Juvix.Compiler.Pipeline
import Juvix.Data.Error qualified as Error
import Juvix.Prelude.Pretty hiding (Doc)
import System.Console.ANSI qualified as Ansi

data App m a where
  ExitMsg :: ExitCode -> Text -> App m a
  ExitJuvixError :: JuvixError -> App m a
  PrintJuvixError :: JuvixError -> App m ()
  AskInvokeDir :: App m (Path Abs Dir)
  AskPkgDir :: App m (Path Abs Dir)
  AskBuildDir :: App m (Path Abs Dir)
  AskPackage :: App m Package
  AskGlobalOptions :: App m GlobalOptions
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: AppPath File -> Sem PipelineEff a -> App m (Either JuvixError (ResolverState, a))
  RunCorePipelineEither :: AppPath File -> Sem TopPipelineEff Core.CoreResult -> App m (Either JuvixError Artifacts)
  Say :: Text -> App m ()
  SayRaw :: ByteString -> App m ()

makeSem ''App

data RunAppIOArgs = RunAppIOArgs
  { _runAppIOArgsGlobalOptions :: GlobalOptions,
    _runAppIOArgsInvokeDir :: Path Abs Dir,
    _runAppIOArgsBuildDir :: Path Abs Dir,
    _runAppIOArgsPkgDir :: Path Abs Dir,
    _runAppIOArgsPkg :: Package
  }

runAppIO ::
  forall r a.
  (Member (Embed IO) r) =>
  RunAppIOArgs ->
  Sem (App ': r) a ->
  Sem r a
runAppIO args@RunAppIOArgs {..} =
  interpret $ \case
    RenderStdOut t
      | _runAppIOArgsGlobalOptions ^. globalOnlyErrors -> return ()
      | otherwise -> embed $ do
          sup <- Ansi.hSupportsANSIColor stdout
          renderIO (not (_runAppIOArgsGlobalOptions ^. globalNoColors) && sup) t
    AskGlobalOptions -> return _runAppIOArgsGlobalOptions
    AskPackage -> return _runAppIOArgsPkg
    AskInvokeDir -> return _runAppIOArgsInvokeDir
    AskPkgDir -> return _runAppIOArgsPkgDir
    AskBuildDir -> return _runAppIOArgsBuildDir
    RunCorePipelineEither input p -> do
      entry <- embed (getEntryPoint' args input)
      embed (pipelineIOEither entry p)
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
    ExitMsg exitCode t -> embed (putStrLn t >> exitWith exitCode)
    SayRaw b -> embed (ByteString.putStr b)
  where
    g :: GlobalOptions
    g = _runAppIOArgsGlobalOptions
    printErr e =
      embed $ hPutStrLn stderr $ run $ runReader (project' @GenericOptions g) $ Error.render (not (_runAppIOArgsGlobalOptions ^. globalNoColors)) (g ^. globalOnlyErrors) e

getEntryPoint' :: RunAppIOArgs -> AppPath File -> IO EntryPoint
getEntryPoint' RunAppIOArgs {..} inputFile = do
  let opts = _runAppIOArgsGlobalOptions
      root = _runAppIOArgsPkgDir
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> getContents
        | otherwise -> return Nothing
  return
    EntryPoint
      { _entryPointRoot = root,
        _entryPointResolverRoot = root,
        _entryPointBuildDir = _runAppIOArgsBuildDir,
        _entryPointNoTermination = opts ^. globalNoTermination,
        _entryPointNoPositivity = opts ^. globalNoPositivity,
        _entryPointNoStdlib = opts ^. globalNoStdlib,
        _entryPointPackage = _runAppIOArgsPkg,
        _entryPointModulePaths = pure (someBaseToAbs _runAppIOArgsInvokeDir (inputFile ^. pathPath)),
        _entryPointGenericOptions = project opts,
        _entryPointStdin = estdin
      }

someBaseToAbs' :: (Members '[App] r) => SomeBase a -> Sem r (Path Abs a)
someBaseToAbs' f = do
  r <- askInvokeDir
  return (someBaseToAbs r f)

askGenericOptions :: (Members '[App] r) => Sem r GenericOptions
askGenericOptions = project <$> askGlobalOptions

getEntryPoint :: (Members '[Embed IO, App] r) => AppPath File -> Sem r EntryPoint
getEntryPoint inputFile = do
  _runAppIOArgsGlobalOptions <- askGlobalOptions
  _runAppIOArgsPkgDir <- askPkgDir
  _runAppIOArgsPkg <- askPackage
  _runAppIOArgsBuildDir <- askBuildDir
  _runAppIOArgsInvokeDir <- askInvokeDir
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
