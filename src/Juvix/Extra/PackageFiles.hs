module Juvix.Extra.PackageFiles where

import Juvix.Data.Effect.Files
import Juvix.Data.Effect.TaggedLock
import Juvix.Extra.Files
import Juvix.Extra.Paths
import Juvix.Prelude

packagePackageFiles :: [(Path Rel File, ByteString)]
packagePackageFiles = juvixFiles $(packageDescriptionDirContents)

packageBaseFiles :: [(Path Rel File, ByteString)]
packageBaseFiles = juvixFiles $(packageBaseDirContents)

writePackageFiles :: forall r. (Members '[Reader OutputRoot, Files] r) => Sem r ()
writePackageFiles = writeFiles packagePackageFiles

writePackageBaseFiles :: forall r. (Members '[Reader OutputRoot, Files] r) => Sem r ()
writePackageBaseFiles = writeFiles packageBaseFiles

updatePackageFiles :: (Members '[TaggedLock, Reader OutputRoot, Files] r) => Sem r ()
updatePackageFiles = updateFiles writePackageFiles

updatePackageBaseFiles :: (Members '[TaggedLock, Reader OutputRoot, Files] r) => Sem r ()
updatePackageBaseFiles = updateFiles writePackageBaseFiles
