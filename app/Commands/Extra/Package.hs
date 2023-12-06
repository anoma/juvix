module Commands.Extra.Package where

import Data.Text.IO.Utf8 qualified as Utf8
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Extra.Paths
import Juvix.Prelude

renderPackage :: Package -> Text
renderPackage = renderPackageVersion currentPackageVersion

writePackageFile' :: (Member (Embed IO) r) => PackageVersion -> Path Abs Dir -> Package -> Sem r ()
writePackageFile' v root pkg =
  embed
    ( Utf8.writeFile @IO
        (toFilePath (root <//> packageFilePath))
        (renderPackageVersion v pkg)
    )

writePackageFile :: (Member (Embed IO) r) => Path Abs Dir -> Package -> Sem r ()
writePackageFile = writePackageFile' currentPackageVersion

writeBasicPackage :: (Member (Embed IO) r) => Path Abs Dir -> Sem r ()
writeBasicPackage root = writePackageFile' PackageBasic root (emptyPackage DefaultBuildDir (root <//> packageFilePath))
