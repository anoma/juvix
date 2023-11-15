module Juvix.Extra.PackageFiles where

import Juvix.Data.Effect.Files
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Files
import Juvix.Extra.Paths
import Juvix.Prelude

packageFiles :: [(Path Rel File, ByteString)]
packageFiles = juvixFiles $(packageDescriptionDirContents)

writePackageFiles :: forall r. (Members '[Reader OutputRoot, Files] r) => Sem r ()
writePackageFiles = writeFiles packageFiles

updatePackageFiles :: (Members '[TaggedLock, Reader OutputRoot, Files] r) => Sem r ()
updatePackageFiles = updateFiles writePackageFiles
