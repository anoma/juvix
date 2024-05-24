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
  GHC.setNumCapabilities (numThreads (_runAppIOArgsGlobalOptions ^. globalNumThreads))
  mbuildDir <- mapM (prepathToAbsDir invokeDir) (_runAppIOArgsGlobalOptions ^? globalBuildDir . _Just . pathPath)
  mainFile <- topCommandInputPath cli
  mapM_ checkMainFile mainFile
  runM
    . runTaggedLockPermissive
    $ do
      _runAppIOArgsRoot <- findRootAndChangeDir (containingDir <$> mainFile) mbuildDir invokeDir
      runAppIO RunAppIOArgs {..} (runTopCommand cli)
  where
    checkMainFile :: SomePath b -> IO ()
    checkMainFile p = unlessM (doesSomePathExist p) err
      where
        err :: IO ()
        err = do
          hPutStrLn stderr [i|The input path #{p} does not exist|]
          exitFailure
