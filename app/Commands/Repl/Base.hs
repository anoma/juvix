module Commands.Repl.Base where

import Commands.Base hiding
  ( command,
  )
import Commands.Repl.Options
import Control.Monad.Except qualified as Except
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict qualified as State
import System.Console.Repline

type ReplS = Reader.ReaderT ReplEnv (State.StateT ReplState (Except.ExceptT JuvixError IO))

type Repl a = HaskelineT ReplS a

data ReplContext = ReplContext
  { _replContextArtifacts :: Artifacts,
    _replContextEntryPoint :: EntryPoint
  }

data ReplEnv = ReplEnv
  { _replRoots :: Roots,
    _replOptions :: ReplOptions
  }

data ReplState = ReplState
  { _replStateRoots :: Roots,
    _replStateContext :: Maybe ReplContext,
    _replStateGlobalOptions :: GlobalOptions
  }

makeLenses ''ReplState
makeLenses ''ReplContext
makeLenses ''ReplEnv
