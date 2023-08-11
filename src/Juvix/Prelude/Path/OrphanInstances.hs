{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted extensions" #-}
{-# HLINT ignore "Avoid restricted flags" #-}

module Juvix.Prelude.Path.OrphanInstances where

import Juvix.Prelude.Base
import Path
import Prettyprinter

instance Pretty (Path a b) where
  pretty = pretty . toFilePath

deriving stock instance (Data b) => Data (SomeBase b)
