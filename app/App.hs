module App where

import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Pipeline
import Juvix.Data.Error qualified as Error
import Juvix.Prelude hiding (Doc)
import Juvix.Prelude.Pretty hiding (Doc)
import System.Console.ANSI qualified as Ansi

data App m a where
  ExitMsg :: ExitCode -> Text -> App m a
  ExitJuvixError :: JuvixError -> App m a
  PrintJuvixError :: JuvixError -> App m ()
  AskRoot :: App m FilePath
  AskPackage :: App m Package
  AskGlobalOptions :: App m GlobalOptions
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: Sem PipelineEff a -> App m (Either JuvixError a)
  Say :: Text -> App m ()
  Raw :: ByteString -> App m ()

makeSem ''App

runAppIO :: forall r a. Member (Embed IO) r => GlobalOptions -> FilePath -> Package -> Sem (App ': r) a -> Sem r a
runAppIO g root pkg = interpret $ \case
  RenderStdOut t
    | g ^. globalOnlyErrors -> return ()
    | otherwise -> embed $ do
        sup <- Ansi.hSupportsANSI stdout
        renderIO (not (g ^. globalNoColors) && sup) t
  AskGlobalOptions -> return g
  AskPackage -> return pkg
  AskRoot -> return root
  RunPipelineEither p -> embed (runIOEither p)
  Say t
    | g ^. globalOnlyErrors -> return ()
    | otherwise -> embed (putStrLn t)
  PrintJuvixError e -> do
    printErr e
  ExitJuvixError e -> do
    printErr e
    embed exitFailure
  ExitMsg exitCode t -> embed (putStrLn t >> exitWith exitCode)
  Raw b -> embed (ByteString.putStr b)
  where
    printErr e =
      embed $ hPutStrLn stderr $ run $ runReader (project' @GenericOptions g) $ Error.render (not (g ^. globalNoColors)) (g ^. globalOnlyErrors) e

runPipeline :: Member App r => Sem PipelineEff a -> Sem r a
runPipeline p = do
  r <- runPipelineEither p
  case r of
    Left err -> exitJuvixError err
    Right res -> return res

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
