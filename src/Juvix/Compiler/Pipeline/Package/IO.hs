module Juvix.Compiler.Pipeline.Package.IO
  ( module Juvix.Compiler.Pipeline.Package.IO,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Prelude

loadPackageFileIO :: (Members '[Error JuvixError, Embed IO] r) => Path Abs Dir -> BuildDir -> Sem r Package
loadPackageFileIO root buildDir =
  runFilesIO
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ loadPackage buildDir (mkPackagePath root)

readPackageIO :: Path Abs Dir -> BuildDir -> IO Package
readPackageIO root buildDir =
  runM
    . runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ readPackage root buildDir

readGlobalPackageIO :: IO Package
readGlobalPackageIO =
  runM
    . runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ readGlobalPackage
