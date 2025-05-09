-- | An effect similar to Polysemy Fail but without an error message
module Juvix.Data.Effect.Fail
  ( module Juvix.Data.Effect.Fail,
    module Effectful.Fail,
  )
where

import Control.Exception qualified as X
import Control.Monad.Fail qualified as Fail
import Effectful.Fail (Fail)
import Effectful.Fail qualified as Fail
import Juvix.Prelude.Base

fail :: (Member Fail r) => Sem r a
fail = Fail.fail "fail"

-- | Run a 'Fail' effect purely.
runFail ::
  Sem (Fail ': r) a ->
  Sem r (Maybe a)
runFail = fmap (^? _Right) . reinterpret (runError @()) (\Fail.Fail {} -> throw ())
{-# INLINE runFail #-}

-- | analogous to <|>
failAlt :: Sem (Fail ': r) a -> Sem (Fail ': r) a -> Sem (Fail ': r) a
failAlt a b = do
  ra <- inject (runFail a)
  maybe b return ra

-- | analogous to `asum`
failAlts :: [Sem (Fail ': r) a] -> Sem (Fail ': r) a
failAlts = \case
  [] -> fail
  x : xs -> failAlt x (failAlts xs)

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
runFailDefaultM defaultVal s = fromMaybeM defaultVal (runFail s)
{-# INLINE runFailDefaultM #-}

ignoreFail ::
  Sem (Fail ': r) a ->
  Sem r ()
ignoreFail = void . runFail
{-# INLINE ignoreFail #-}

failWhen :: (Member Fail r) => Bool -> Sem r ()
failWhen c = when c fail
{-# INLINE failWhen #-}

failWhenM :: (Member Fail r) => Sem r Bool -> Sem r ()
failWhenM c = whenM c fail
{-# INLINE failWhenM #-}

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
  (Members '[Fail, EmbedIO] r) =>
  IO a ->
  Sem r a
failFromException m = do
  r <- liftIO (X.try @X.SomeException m)
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
