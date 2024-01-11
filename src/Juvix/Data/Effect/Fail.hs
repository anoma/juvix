-- | An effect similar to Polysemy Fail but without an error message
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

-- | Run a 'Fail' effect purely with a default value.
runFailDefault ::
  a ->
  Sem (Fail ': r) a ->
  Sem r a
runFailDefault defaultVal = runFailDefaultM (return defaultVal)
{-# INLINE runFailDefault #-}

-- | Run a 'Fail' effect with a default value.
runFailDefaultM ::
  Sem r a ->
  Sem (Fail ': r) a ->
  Sem r a
runFailDefaultM defaultVal s = do
  x <- runError @() (reinterpret (\Fail -> throw ()) s)
  case x of
    Left {} -> defaultVal
    Right y -> return y
{-# INLINE runFailDefaultM #-}

ignoreFail ::
  Sem (Fail ': r) a ->
  Sem r ()
ignoreFail = void . runFail
{-# INLINE ignoreFail #-}

failWhen :: (Member Fail r) => Bool -> Sem r ()
failWhen c = when c fail
{-# INLINE failWhen #-}

failUnlessM :: (Member Fail r) => Sem r Bool -> Sem r ()
failUnlessM c = unlessM c fail
{-# INLINE failUnlessM #-}

failUnless :: (Member Fail r) => Bool -> Sem r ()
failUnless c = unless c fail
{-# INLINE failUnless #-}

failMaybe :: (Member Fail r) => Maybe a -> Sem r a
failMaybe = \case
  Nothing -> fail
  Just x -> return x
{-# INLINE failMaybe #-}

failFromException ::
  (Members '[Fail, Embed IO] r) =>
  IO a ->
  Sem r a
failFromException m = do
  r <- embed (X.try @X.SomeException m)
  case r of
    Left {} -> fail
    Right a -> return a

failFromError :: forall e r a. (Member Fail r) => Sem (Error e ': r) a -> Sem r a
failFromError s = do
  res <- runError s
  case res of
    Left {} -> fail
    Right x -> return x

failWithError :: forall e r a. (Member (Error e) r) => e -> Sem (Fail ': r) a -> Sem r a
failWithError err s = do
  res <- runFail s
  case res of
    Nothing -> throw err
    Just x -> return x
