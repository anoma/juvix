module Juvix.Compiler.Pipeline.Loader.PathResolver.ImportTree.ImportNode where

import Juvix.Prelude
import Juvix.Prelude.Pretty

data ImportNode = ImportNode
  { _importNodePackageRoot :: Path Abs Dir,
    _importNodeFile :: Path Rel File
  }
  deriving stock (Eq, Ord, Generic, Show)

instance Pretty ImportNode where
  pretty ImportNode {..} = pretty _importNodePackageRoot <+> ":" <+> show _importNodeFile

instance Hashable ImportNode

makeLenses ''ImportNode

importNodeAbsFile :: SimpleGetter ImportNode (Path Abs File)
importNodeAbsFile = to $ \ImportNode {..} -> _importNodePackageRoot <//> _importNodeFile
