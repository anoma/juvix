module Juvix.Prelude.Path.SomePath where

import Juvix.Prelude.Base
import Path
import Path.IO
import Prelude qualified

data SomePath b
  = File (Path b File)
  | Dir (Path b Dir)

-- | Return the directory containing a path. i.e the path itself if it is a
-- directory or its parent if it is a file.
containingDir :: SomePath b -> Path b Dir
containingDir = \case
  File f -> parent f
  Dir d -> d

doesSomePathExist :: SomePath b -> IO Bool
doesSomePathExist = \case
  File p -> doesPathExist p
  Dir p -> doesPathExist p

instance Show (SomePath b) where
  show = \case
    File p -> show p
    Dir p -> show p
