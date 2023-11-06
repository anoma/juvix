module Commands.Extra.Package where

import Data.Text.IO.Utf8 qualified as Utf8
import Juvix.Compiler.Pipeline.Package.Base
import Juvix.Compiler.Pipeline.Package.Loader
import Juvix.Extra.Paths
import Juvix.Prelude

renderPackage :: Package -> Text
renderPackage = renderPackageVersion PackageVersion1

writePackageFile :: (Member (Embed IO) r) => Path Abs Dir -> Package -> Sem r ()
writePackageFile root pkg =
  embed
    ( Utf8.writeFile @IO
        (toFilePath (root <//> packageFilePath))
        (renderPackage pkg)
    )
