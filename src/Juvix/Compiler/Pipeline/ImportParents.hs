module Juvix.Compiler.Pipeline.ImportParents where

import Juvix.Compiler.Concrete.Data
import Juvix.Prelude.Base

data ImportParents = ImportParents
  { _importParents :: [TopModulePath],
    _importParentsSeenModules :: HashSet TopModulePath
  }

makeLenses ''ImportParents

instance Semigroup ImportParents where
  s1 <> s2 =
    ImportParents
      { _importParents = s1 ^. importParents <> s2 ^. importParents,
        _importParentsSeenModules = s1 ^. importParentsSeenModules <> s2 ^. importParentsSeenModules
      }

instance Monoid ImportParents where
  mempty =
    ImportParents
      { _importParents = mempty,
        _importParentsSeenModules = mempty
      }
