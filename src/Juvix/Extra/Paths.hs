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

cssDir :: [(Path Rel File, ByteString)]
cssDir = map (first relFile) $(cssDirQ)

jsDir :: [(Path Rel File, ByteString)]
jsDir = map (first relFile) $(jsDirQ)

imagesDir :: [(Path Rel File, ByteString)]
imagesDir = map (first relFile) $(imagesDirQ)

-- | Given a relative file from the root of the project, checks that the file
-- exists and returns the absolute path
mkProjFile :: Path Rel File -> Q Exp
mkProjFile r = do
  let p = relToProject r
  ensureFile p
  lift p

-- | imaginary file path for error messages in the repl.
replPath :: Path Abs File
replPath = $(mkAbsFile "/repl")

formatStdinPath :: Path Abs File
formatStdinPath = $(mkAbsFile "/format-stdin")

gebReplPath :: Path Abs File
gebReplPath = $(mkAbsFile "/repl.geb")
