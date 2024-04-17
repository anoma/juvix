module Main (main) where

import App
import CommonOptions
import Data.String.Interpolate (i)
import GlobalOptions
import Juvix.Compiler.Pipeline.Root
import Parallel.Experiment qualified as Experiment
import TopCommand
import TopCommand.Options

main :: IO ()
main = Experiment.main

main2 :: IO ()
main2 = do
  let parserPreferences = prefs showHelpOnEmpty
  invokeDir <- getCurrentDir
  (_runAppIOArgsGlobalOptions, cli) <- customExecParser parserPreferences descr
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
