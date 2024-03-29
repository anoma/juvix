module Commands.Extra.Package where

import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Extra.Paths
import Juvix.Prelude

renderPackage :: Package -> Text
renderPackage = renderPackageVersion currentPackageVersion

writePackageFile' :: (Member EmbedIO r) => PackageVersion -> Path Abs Dir -> Package -> Sem r ()
writePackageFile' v root pkg =
  writeFileEnsureLn
    (root <//> packageFilePath)
    (renderPackageVersion v pkg)

writePackageFile :: (Member EmbedIO r) => Path Abs Dir -> Package -> Sem r ()
writePackageFile = writePackageFile' currentPackageVersion

writeBasicPackage :: (Member EmbedIO r) => Path Abs Dir -> Sem r ()
writeBasicPackage root = writePackageFile' PackageBasic root (emptyPackage DefaultBuildDir (root <//> packageFilePath))
