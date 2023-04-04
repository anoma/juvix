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
  _runAppIOArgsInvokeDir <- getCurrentDir
  (_runAppIOArgsGlobalOptions, cli) <- customExecParser parserPreferences descr
  let mbuildDir = _runAppIOArgsGlobalOptions ^? globalBuildDir . _Just . pathPath
  roots <- findRootAndChangeDir (topCommandInputFile cli) mbuildDir _runAppIOArgsInvokeDir
  let runAppArgs =
        RunAppIOArgs
          { _runAppIOArgsPkgGlobal = roots ^. rootsPackageGlobal,
            _runAppIOArgsPkgDir = roots ^. rootsRootDir,
            _runAppIOArgsPkg = roots ^. rootsPackage,
            _runAppIOArgsBuildDir = roots ^. rootsBuildDir,
            _runAppIOArgsInvokeDir,
            _runAppIOArgsGlobalOptions
          }
  runFinal (resourceToIOFinal (embedToFinal @IO (runAppIO runAppArgs (runTopCommand cli))))
