module Commands.Clean where

import Commands.Base
import Commands.Clean.Options

runCommand :: (Members '[Files, App] r) => CleanOptions -> Sem r ()
runCommand opts
  | opts ^. cleanOptionsOnlyGlobal = do
      cleanGlobal
  | opts ^. cleanOptionsGlobal = do
      cleanGlobal
      cleanLocal
  | otherwise = do
      cleanLocal

cleanGlobal :: (Members '[Files, App] r) => Sem r ()
cleanGlobal = do
  configDir <- juvixConfigDir
  whenM (directoryExists' configDir) (removeDirectoryRecursive' configDir)

cleanLocal :: (Members '[Files, App] r) => Sem r ()
cleanLocal = do
  buildDir <- askBuildDir
  whenM (directoryExists' buildDir) (removeDirectoryRecursive' buildDir)
