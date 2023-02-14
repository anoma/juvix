module Juvix.Compiler.Core.Transformation.MatchToCase.Data where

import Juvix.Compiler.Core.Extra
import Juvix.Compiler.Core.Language

-- | A CompiledBinder is either a binder that was present in the original match (OriginalBinder)
-- or an additional binder that was added during the compilation (AuxiliaryBinder)
data CompiledBinder where
  OriginalBinder :: Binder -> CompiledBinder
  AuxiliaryBinder :: Binder -> CompiledBinder

isOriginalBinder :: CompiledBinder -> Bool
isOriginalBinder = \case
  OriginalBinder {} -> True
  AuxiliaryBinder {} -> False

isAuxiliaryBinder :: CompiledBinder -> Bool
isAuxiliaryBinder = \case
  OriginalBinder {} -> False
  AuxiliaryBinder {} -> True

getBinder :: CompiledBinder -> Binder
getBinder = \case
  AuxiliaryBinder b -> b
  OriginalBinder b -> b

-- | A CompiledPattern is the result of compiling a single Pattern of a match.
--
-- CompiledPatterns can be composed using the `compiledPatMkNode` function.
--
-- The `compiledPatBinders` field records the binders that were indroduced by
-- the compilation in the order that they were added.
data CompiledPattern = CompiledPattern
  { -- | The binders added during compilation
    _compiledPatBinders :: [CompiledBinder],
    -- | A function to construct the compiled Node wrapping an already compiled
    -- Node
    _compiledPatMkNode :: Node -> Node
  }

data CompileState = CompileState
  { _compileStateBindersAbove :: Int,
    _compileStateCompiledPattern :: CompiledPattern,
    _compileStateCurrentNode :: Node
  }

initCompiledPattern :: CompiledPattern
initCompiledPattern =
  CompiledPattern
    { _compiledPatBinders = [],
      _compiledPatMkNode = id
    }

initState :: CompileState
initState =
  CompileState
    { _compileStateBindersAbove = 0,
      _compileStateCompiledPattern =
        CompiledPattern
          { _compiledPatBinders = [],
            _compiledPatMkNode = id
          },
      _compileStateCurrentNode = mkVar' 0
    }

stateWithBindersAbove :: Int -> CompileState
stateWithBindersAbove n = initState {_compileStateBindersAbove = n}

makeLenses ''CompiledPattern
makeLenses ''CompileState

incBindersAbove :: Member (Reader CompileState) r => Sem r CompiledPattern -> Sem r CompiledPattern
incBindersAbove = local (over compileStateBindersAbove (+ 1))

resetCurrentNode :: Member (Reader CompileState) r => Sem r CompiledPattern -> Sem r CompiledPattern
resetCurrentNode = local (set compileStateCurrentNode (mkVar' 0))

instance Semigroup CompiledPattern where
  cp1 <> cp2 = CompiledPattern {_compiledPatBinders=(cp1 ^. compiledPatBinders) <> (cp2 ^. compiledPatBinders),
                                _compiledPatMkNode=(cp1 ^. compiledPatMkNode) . (cp2 ^. compiledPatMkNode)}
