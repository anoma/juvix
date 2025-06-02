module Commands.Dev.MigrateJuvixYaml where

import Commands.Base
import Commands.Dev.MigrateJuvixYaml.Options
import Commands.Extra.Package
import Juvix.Extra.Paths

runCommand :: forall r. (Members '[EmbedIO, Files, App] r) => MigrateJuvixYamlOptions -> Sem r ()
runCommand MigrateJuvixYamlOptions {..} = do
  pkgDir <- askPkgDir
  isGlobalPackage <- askPackageGlobal
  let pkgFilePath = pkgDir <//> packageFilePath
  pkgFileExists <- fileExists' pkgFilePath
  pkg <- askPackage
  if
    | isGlobalPackage -> exitMsg (ExitFailure 1) "No Package file found"
    | not pkgFileExists || _migrateJuvixYamlOptionsForce -> do
        writePackageFile pkgDir pkg
        removeFile' (pkgDir <//> juvixYamlFile)
    | otherwise -> exitMsg (ExitFailure 1) (show pkgFilePath <> " already exists.")
