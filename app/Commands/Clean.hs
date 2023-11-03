module Commands.Clean where

import Commands.Base
import Commands.Clean.Options

runCommand :: (Members '[Files, App] r) => CleanOptions -> Sem r ()
runCommand opts
  | opts ^. cleanOptionsGlobal = do
      configDir <- juvixConfigDir
      whenM (directoryExists' configDir) (removeDirectoryRecursive' configDir)
  | otherwise = do
      buildDir <- askBuildDir
      whenM (directoryExists' buildDir) (removeDirectoryRecursive' buildDir)
