-- | An effect similar to Polysemy Fail but wihout an error message
module Juvix.Data.Effect.Fail where

import Control.Exception qualified as X
import Juvix.Prelude.Base

data Fail m a = Fail

makeSem ''Fail

-- | Run a 'Fail' effect purely.
runFail ::
  Sem (Fail ': r) a ->
  Sem r (Maybe a)
runFail = fmap (^? _Right) . runError @() . reinterpret (\Fail -> throw ())
{-# INLINE runFail #-}

ignoreFail ::
  Sem (Fail ': r) a ->
  Sem r ()
ignoreFail = void . runFail
{-# INLINE ignoreFail #-}

failMaybe :: (Member Fail r) => Maybe a -> Sem r a
failMaybe = \case
  Nothing -> fail
  Just x -> return x

failFromException ::
  (Members '[Fail, Embed IO] r) =>
  IO a ->
  Sem r a
failFromException m = do
  r <- embed (X.try @X.SomeException m)
  case r of
    Left {} -> fail
    Right a -> return a
