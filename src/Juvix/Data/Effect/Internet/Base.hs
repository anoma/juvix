-- | This effect indicates whether we can use the internet or not.
module Juvix.Data.Effect.Internet.Base (
  InternetWitness,
  Online,
  Internet,
  getInternet,
  evalInternetOnline,
  evalInternetOffline,
  evalInternet,
  whenHasInternet,
  ifHasInternet,
  ) where

import Juvix.Prelude.Base

data InternetWitness = InternetWitness

type Online = Reader InternetWitness

data Internet m a where
  -- | Returns `Nothing` if we are offline
  GetInternet :: Internet m (Maybe InternetWitness)

makeSem ''Internet

ifHasInternet :: Members '[Internet] r => Sem (Online ': r) () -> Sem r () -> Sem r ()
ifHasInternet ifonline ifoffline = do
  x <- getInternet
  case x of
    Nothing -> ifoffline
    Just w -> runReader w ifonline

whenHasInternet :: Members '[Internet] r => Sem (Online ': r) () -> Sem r ()
whenHasInternet m = ifHasInternet m (return ())

evalInternet :: Bool -> Sem (Internet ': r) a -> Sem r a
evalInternet hasInternet
 | hasInternet = evalInternetOnline
 | otherwise = evalInternetOffline

evalInternetOffline :: Sem (Internet ': r) a -> Sem r a
evalInternetOffline = interpret $ \case
  GetInternet -> return Nothing

evalInternetOnline :: Sem (Internet ': r) a -> Sem r a
evalInternetOnline = interpret $ \case
  GetInternet -> return (Just InternetWitness)
