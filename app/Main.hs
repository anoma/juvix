module Main (main) where

import App
import CommonOptions
import GlobalOptions
import Juvix.Compiler.Pipeline.Root
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let parserPreferences = prefs showHelpOnEmpty
  invokeDir <- getCurrentDir
  (_runAppIOArgsGlobalOptions, cli) <- customExecParser parserPreferences descr
  let mbuildDir = _runAppIOArgsGlobalOptions ^? globalBuildDir . _Just . pathPath
  mainFileDir <- topCommandInputFile cli
  _runAppIOArgsRoots <- findRootAndChangeDir mainFileDir mbuildDir invokeDir
  runFinal (resourceToIOFinal (embedToFinal @IO (runAppIO RunAppIOArgs {..} (runTopCommand cli))))
