module Main (main) where

import App
import CommonOptions
import Root
import TopCommand
import TopCommand.Options

main :: IO ()
main = do
  let p = prefs showHelpOnEmpty
  (global, cli) <- customExecParser p descr
  (root, pkg) <- findRoot (topCommandInputFile cli)
  runM (runAppIO global root pkg (runTopCommand cli))
