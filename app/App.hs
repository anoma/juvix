module App where

import GlobalOptions
import MiniJuvix.Pipeline
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Prelude.Pretty hiding (Doc)
import System.Console.ANSI qualified as Ansi

data App m a where
  ExitError :: AJuvixError -> App m a
  Say :: Text -> App m ()
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: Sem PipelineEff a -> App m (Either AJuvixError a)
  ReadGlobalOptions :: App m GlobalOptions

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
  ExitError e -> do
    whenJust (genericError e) (embed . hPutStrLn stderr . renderGenericError (not (g ^. globalNoColors)))
    embed exitFailure

runPipeline :: Member App r => Sem PipelineEff a -> Sem r a
runPipeline p = do
  r <- runPipelineEither p
  case r of
    Left err -> exitError err
    Right res -> return res

newline :: Member App r => Sem r ()
newline = say ""
