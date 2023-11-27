module Juvix.Compiler.Pipeline.Package.IO
  ( module Juvix.Compiler.Pipeline.Package.IO,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

loadPackageFileIO :: (Members '[TaggedLock, Error JuvixError, Embed IO] r) => Path Abs Dir -> BuildDir -> Sem r Package
loadPackageFileIO root buildDir =
  runFilesIO
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ loadPackage buildDir (mkPackagePath root)

readPackageIO :: LockMode -> Path Abs Dir -> BuildDir -> IO Package
readPackageIO lockMode root buildDir =
  runFinal
    . resourceToIOFinal
    . embedToFinal @IO
    . runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runTaggedLock lockMode
    . runEvalFileEffIO
    $ readPackage root buildDir

readGlobalPackageIO :: LockMode -> IO Package
readGlobalPackageIO lockMode =
  runFinal
    . resourceToIOFinal
    . embedToFinal @IO
    . runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runTaggedLock lockMode
    . runEvalFileEffIO
    $ readGlobalPackage
