module Juvix.Extra.Paths
  ( module Juvix.Extra.Paths,
    module Juvix.Extra.Paths.Base,
  )
where

import Juvix.Extra.Paths.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path

relToProject :: Path Rel a -> Path Abs a
relToProject r = $(projectPath) <//> r

assetsDir :: [(Path Rel File, ByteString)]
assetsDir = map (first relFile) $(assetsDirQ)
