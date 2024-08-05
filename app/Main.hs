module Main (main) where

import App
import CommonOptions
import Data.String.Interpolate (i)
import GHC.Conc qualified as GHC
import GlobalOptions
import Juvix.Compiler.Pipeline.Root
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let parserPreferences = prefs showHelpOnEmpty
  invokeDir <- getCurrentDir
  (_runAppIOArgsGlobalOptions, cli) <- customExecParser parserPreferences descr
  numThreads (_runAppIOArgsGlobalOptions ^. globalNumThreads) >>= GHC.setNumCapabilities
  mbuildDir <- mapM (prepathToAbsDir invokeDir) (_runAppIOArgsGlobalOptions ^? globalBuildDir . _Just . pathPath)
  mainFile <- topCommandInputPath cli
  let loggerOpts =
        LoggerOptions
          { _loggerLevel = _runAppIOArgsGlobalOptions ^. globalLogLevel,
            _loggerUseColors = not (_runAppIOArgsGlobalOptions ^. globalNoColors)
          }
  runM
    . runTaggedLockPermissive
    . runLoggerIO loggerOpts
    . runFilesIO
    $ do
      mapM_ checkMainFile mainFile
      _runAppIOArgsRoot <- findRootAndChangeDir (containingDir <$> mainFile) mbuildDir invokeDir
      runAppIO RunAppIOArgs {..} (runTopCommand cli)
  where
    checkMainFile :: forall r b. (Members '[Logger, EmbedIO] r) => SomePath b -> Sem r ()
    checkMainFile p = unlessM (liftIO (doesSomePathExist p)) err
      where
        err :: Sem r ()
        err = do
          logError (mkAnsiText @Text [i|The input path #{p} does not exist|])
          exitFailure
