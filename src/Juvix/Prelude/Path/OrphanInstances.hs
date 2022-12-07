{-# OPTIONS_GHC -Wno-orphans #-}
module Juvix.Prelude.Path.OrphanInstances where

import Path
import Juvix.Prelude.Base
import Prettyprinter

instance Pretty (Path a b) where
  pretty = pretty . toFilePath
