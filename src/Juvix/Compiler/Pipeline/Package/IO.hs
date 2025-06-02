module Juvix.Compiler.Pipeline.Package.IO
  ( module Juvix.Compiler.Pipeline.Package.IO,
    module Juvix.Compiler.Pipeline.Package,
  )
where

import Juvix.Compiler.Pipeline.EntryPoint
import Juvix.Compiler.Pipeline.Package
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Compiler.Pipeline.Package.Loader.Error
import Juvix.Compiler.Pipeline.Package.Loader.EvalEff.IO
import Juvix.Data.Effect.TaggedLock
import Juvix.Prelude

loadPackageFileIO :: (Members '[TaggedLock, Error JuvixError, Reader EntryPoint, EmbedIO] r) => Path Abs Dir -> BuildDir -> Sem r Package
loadPackageFileIO root buildDir =
  runFilesIO
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ loadPackage buildDir (mkPackagePath root)

readPackageIO :: (Members '[TaggedLock, Reader EntryPoint, EmbedIO] r) => Path Abs Dir -> BuildDir -> Sem r Package
readPackageIO root buildDir =
  runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ readPackage root buildDir

readGlobalPackageIO :: (Members '[EmbedIO, Reader EntryPoint, TaggedLock] r) => Sem r Package
readGlobalPackageIO =
  runFilesIO
    . runErrorIO' @JuvixError
    . mapError (JuvixError @PackageLoaderError)
    . runEvalFileEffIO
    $ readGlobalPackage
