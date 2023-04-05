{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module gives a minimal environment to run Traces, best used
-- for testing
module Mari.Library.Trace.Environment where

import Mari.Library
import Mari.Library.Trace.Types

newtype Minimal = Minimal
  { trace :: T
  }
  deriving (Generic, Show)

type MinimalAlias = State Minimal

type MinimalAliasIO = StateT Minimal IO

newtype MinimalM a = Ctx {_run :: MinimalAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasState "trace" T,
      HasSource "trace" T,
      HasSink "trace" T
    )
    via StateField "trace" MinimalAlias

newtype MinimalMIO a = CtxIO (MinimalAliasIO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasState "trace" T,
      HasSource "trace" T,
      HasSink "trace" T
    )
    via StateField "trace" MinimalAliasIO

run :: MinimalM a -> Minimal -> (a, Minimal)
run (Ctx c) = runState c

runEmpty :: MinimalM a -> (a, Minimal)
runEmpty c = run c (Minimal (T Empty [] mempty Nothing))

runIO :: MinimalMIO a -> Minimal -> IO (a, Minimal)
runIO (CtxIO c) = runStateT c

runEmptyIO :: MinimalMIO a -> IO (a, Minimal)
runEmptyIO c = runIO c (Minimal (T Empty [] mempty Nothing))

runEmptyTraceAll :: MinimalM a -> (a, Minimal)
runEmptyTraceAll c = run c (Minimal (T Empty [] mempty (Just 10000)))

runEmptyTraceAllIO :: MinimalMIO a -> IO (a, Minimal)
runEmptyTraceAllIO c = runIO c (Minimal (T Empty [] mempty (Just 10000)))
