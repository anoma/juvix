module App where

import Data.ByteString qualified as ByteString
import GlobalOptions
import Juvix.Compiler.Pipeline
import Juvix.Data.Error qualified as Error
import Juvix.Prelude hiding (Doc)
import Juvix.Prelude.Pretty hiding (Doc)
import System.Console.ANSI qualified as Ansi

data App m a where
  ExitMsg :: ExitCode -> Text -> App m ()
  ExitJuvixError :: GenericOptions -> JuvixError -> App m a
  PrintJuvixError :: GenericOptions -> JuvixError -> App m ()
  ReadGlobalOptions :: App m GlobalOptions
  RenderStdOut :: (HasAnsiBackend a, HasTextBackend a) => a -> App m ()
  RunPipelineEither :: Sem PipelineEff a -> App m (Either JuvixError a)
  Say :: Text -> App m ()
  Raw :: ByteString -> App m ()

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
  PrintJuvixError opts e -> do
    printErr opts e
  ExitJuvixError opts e -> do
    printErr opts e
    embed exitFailure
  ExitMsg exitCode t -> embed (putStrLn t >> exitWith exitCode)
  Raw b -> embed (ByteString.putStr b)
  where
    printErr opts e =
      (embed . hPutStrLn stderr . Error.render opts (not (g ^. globalNoColors)) (g ^. globalOnlyErrors)) e

runPipeline :: Member App r => GenericOptions -> Sem PipelineEff a -> Sem r a
runPipeline opts p = do
  r <- runPipelineEither p
  case r of
    Left err -> exitJuvixError opts err
    Right res -> return res

newline :: Member App r => Sem r ()
newline = say ""

printSuccessExit :: Member App r => Text -> Sem r ()
printSuccessExit = exitMsg ExitSuccess

printFailureExit :: Member App r => Text -> Sem r ()
printFailureExit = exitMsg (ExitFailure 1)
