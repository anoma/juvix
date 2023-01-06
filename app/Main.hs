module Main (main) where

import App
import CommonOptions
import Root
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let p = prefs showHelpOnEmpty
  cwd <- getCurrentDir
  (global, cli) <- customExecParser p descr
  (root, pkg, buildDir) <- findRootAndChangeDir (topCommandInputFile cli) global
  runM (runAppIO global cwd root buildDir pkg (runTopCommand cli))
