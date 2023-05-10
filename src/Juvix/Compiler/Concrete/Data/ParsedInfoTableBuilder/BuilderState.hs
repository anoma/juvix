module Juvix.Compiler.Concrete.Data.ParsedInfoTableBuilder.BuilderState where

import Juvix.Compiler.Concrete.Language
import Juvix.Prelude

data BuilderState = BuilderState
  { _stateComments :: [SpaceSpan],
    _stateVisited :: HashSet TopModulePath,
    _stateModules :: HashMap TopModulePath (Module 'Parsed 'ModuleTop)
  }
  deriving stock (Show)

makeLenses ''BuilderState

iniState :: BuilderState
iniState =
  BuilderState
    { _stateComments = [],
      _stateVisited = mempty,
      _stateModules = mempty
    }
