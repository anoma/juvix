module App where

import GlobalOptions
import MiniJuvix.Pipeline
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Prelude.Error qualified as Error
import MiniJuvix.Prelude.Pretty hiding (Doc)
import System.Console.ANSI qualified as Ansi

data App m a where
  ExitMsg :: ExitCode -> Text -> App m ()
  ExitMiniJuvixError :: MiniJuvixError -> App m a
  ReadGlobalOptions :: App m GlobalOptions
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: Sem PipelineEff a -> App m (Either MiniJuvixError a)
  Say :: Text -> App m ()

makeSem ''App

runAppIO :: forall r a. Member (Embed IO) r => GlobalOptions -> Sem (App ': r) a -> Sem r a
runAppIO g = interpret $ \case
  RenderStdOut t
    | g ^. globalOnlyErrors -> return ()
    | otherwise -> embed $ do
        sup <- Ansi.hSupportsANSI stdout
        renderIO (not (g ^. globalNoColors) && sup) t
  ReadGlobalOptions -> return g
  RunPipelineEither p -> embed (runIOEither p)
  Say t
    | g ^. globalOnlyErrors -> return ()
    | otherwise -> embed (putStrLn t)
  ExitMiniJuvixError e -> do
    (embed . hPutStrLn stderr . Error.render (not (g ^. globalNoColors))) e
    embed exitFailure
  ExitMsg exitCode t -> embed (putStrLn t >> exitWith exitCode)

runPipeline :: Member App r => Sem PipelineEff a -> Sem r a
runPipeline p = do
  r <- runPipelineEither p
  case r of
    Left err -> exitMiniJuvixError err
    Right res -> return res

newline :: Member App r => Sem r ()
newline = say ""

printSuccessExit :: Member App r => Text -> Sem r ()
printSuccessExit = exitMsg ExitSuccess

printFailureExit :: Member App r => Text -> Sem r ()
printFailureExit = exitMsg (ExitFailure 1)
