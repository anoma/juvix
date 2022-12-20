module Juvix.Extra.Paths
  ( module Juvix.Extra.Paths,
    module Juvix.Extra.Paths.Base,
  )
where

import Juvix.Extra.Paths.Base
import Juvix.Prelude.Base
import Juvix.Prelude.Path
import Language.Haskell.TH.Syntax

relToProject :: Path Rel a -> Path Abs a
relToProject r = $(projectPath) <//> r

assetsDir :: [(Path Rel File, ByteString)]
assetsDir = map (first relFile) $(assetsDirQ)

-- | Given a relative file from the root of the project, checks that the file
-- exists and returns the absolute path
mkProjFile :: Path Rel File -> Q Exp
mkProjFile r = do
  let p = relToProject r
  ensureFile p
  lift p
