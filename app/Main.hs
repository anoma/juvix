module Main (main) where

import App
import CommonOptions
import Root
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let parserPreferences = prefs showHelpOnEmpty
  _runAppIOArgsInvokeDir <- getCurrentDir
  (_runAppIOArgsGlobalOptions, cli) <- customExecParser parserPreferences descr
  (_runAppIOArgsPkgDir, _runAppIOArgsPkg, _runAppIOArgsBuildDir) <- findRootAndChangeDir (topCommandInputFile cli) _runAppIOArgsGlobalOptions _runAppIOArgsInvokeDir
  runM (runAppIO (RunAppIOArgs {..}) (runTopCommand cli))
