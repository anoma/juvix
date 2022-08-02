-- | An effect similar to Polysemy Fail but wihout an error message
module Juvix.Data.Effect.Fail where

import Juvix.Prelude.Base

data Fail m a = Fail

makeSem ''Fail

-- | Run a 'Fail' effect purely.
runFail ::
  Sem (Fail ': r) a ->
  Sem r (Maybe a)
runFail = fmap (^? _Right) . runError @() . reinterpret (\Fail -> throw ())
{-# INLINE runFail #-}

failMaybe :: Member Fail r => Maybe a -> Sem r a
failMaybe = \case
  Nothing -> fail
  Just x -> return x
