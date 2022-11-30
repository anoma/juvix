module Juvix.Prelude.Path
  ( module Juvix.Prelude.Path,
    module Path,
  )
where

import Juvix.Prelude.Base
import Path hiding ((</>))
import Path qualified

-- | Synonym for Path.</>. Useful to avoid name clashes
infixr 5 <//>

(<//>) :: Path b Dir -> Path Rel t -> Path b t
(<//>) = (Path.</>)

relFile :: FilePath -> Path Rel File
relFile = fromJust . parseRelFile

relDir :: FilePath -> Path Rel Dir
relDir = fromJust . parseRelDir

destructPath :: Path b Dir -> [Path Rel Dir]
destructPath p = map relDir (splitPath (toFilePath p))

destructFilePath :: Path b File -> ([Path Rel Dir], Path Rel File)
destructFilePath p = case nonEmptyUnsnoc (nonEmpty' (splitPath (toFilePath p))) of
  (ps, f) -> (fmap relDir (maybe [] toList ps), relFile f)
