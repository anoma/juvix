module App where

import CommonOptions
import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Builtins.Effect
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
  AskPackage :: App m Package
  AskGlobalOptions :: App m GlobalOptions
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: AppPath File -> Sem PipelineEff a -> App m (Either JuvixError (Artifacts, a))
  Say :: Text -> App m ()
  SayRaw :: ByteString -> App m ()

makeSem ''App

runAppIO :: forall r a. Member (Embed IO) r => GlobalOptions -> Path Abs Dir -> Path Abs Dir -> Package -> Sem (App ': r) a -> Sem r a
runAppIO g invokeDir pkgDir pkg = interpret $ \case
  RenderStdOut t
    | g ^. globalOnlyErrors -> return ()
    | otherwise -> embed $ do
        sup <- Ansi.hSupportsANSIColor stdout
        renderIO (not (g ^. globalNoColors) && sup) t
  AskGlobalOptions -> return g
  AskPackage -> return pkg
  AskInvokeDir -> return invokeDir
  AskPkgDir -> return pkgDir
  RunPipelineEither input p -> do
    entry <- embed (getEntryPoint' invokeDir g pkgDir pkg input)
    embed (runIOEither iniState entry p)
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
    printErr e =
      embed $ hPutStrLn stderr $ run $ runReader (project' @GenericOptions g) $ Error.render (not (g ^. globalNoColors)) (g ^. globalOnlyErrors) e

getEntryPoint' :: Path Abs Dir -> GlobalOptions -> Path Abs Dir -> Package -> AppPath File -> IO EntryPoint
getEntryPoint' invokeDir opts root pkg inputFile = do
  estdin <-
    if
        | opts ^. globalStdin -> Just <$> getContents
        | otherwise -> return Nothing
  return
    EntryPoint
      { _entryPointRoot = root,
        _entryPointNoTermination = opts ^. globalNoTermination,
        _entryPointNoPositivity = opts ^. globalNoPositivity,
        _entryPointNoStdlib = opts ^. globalNoStdlib,
        _entryPointStdlibPath = someBaseToAbs invokeDir <$> opts ^. globalStdlibPath,
        _entryPointPackage = pkg,
        _entryPointModulePaths = pure (someBaseToAbs invokeDir (inputFile ^. pathPath)),
        _entryPointGenericOptions = project opts,
        _entryPointStdin = estdin
      }

someBaseToAbs' :: Members '[App] r => SomeBase a -> Sem r (Path Abs a)
someBaseToAbs' f = do
  r <- askInvokeDir
  return (someBaseToAbs r f)

askGenericOptions :: Members '[App] r => Sem r GenericOptions
askGenericOptions = project <$> askGlobalOptions

getEntryPoint :: Members '[Embed IO, App] r => AppPath File -> Sem r EntryPoint
getEntryPoint inputFile = do
  opts <- askGlobalOptions
  root <- askPkgDir
  pkg <- askPackage
  invokeDir <- askInvokeDir
  embed (getEntryPoint' invokeDir opts root pkg inputFile)

runPipeline :: Member App r => AppPath File -> Sem PipelineEff a -> Sem r a
runPipeline input p = do
  r <- runPipelineEither input p
  case r of
    Left err -> exitJuvixError err
    Right res -> return (snd res)

newline :: Member App r => Sem r ()
newline = say ""

printSuccessExit :: Member App r => Text -> Sem r a
printSuccessExit = exitMsg ExitSuccess

printFailureExit :: Member App r => Text -> Sem r a
printFailureExit = exitMsg (ExitFailure 1)

getRight :: (Members '[App] r, AppError e) => Either e a -> Sem r a
getRight = either appError return

instance AppError Text where
  appError = printFailureExit

instance AppError JuvixError where
  appError = exitJuvixError

class AppError e where
  appError :: Members '[App] r => e -> Sem r a
