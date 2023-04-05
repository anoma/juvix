{-# LANGUAGE MultiWayIf #-}

module Trace where

import Mari.Library
import qualified Mari.Library.Trace as Trace
import qualified Mari.Library.Trace.Environment as Env

add1 :: (Trace.Eff m, Show b, Enum b) => b -> m b
add1 n =
  Trace.withScope "Prelude.add1" [show n] $ do
    pure (succ n)

add2 :: (Trace.Eff m, Show b, Enum b) => b -> m b
add2 n =
  Trace.withScope "Prelude.add2" [show n] $ do
    n <- add1 n
    add1 n

addBreak :: (Trace.Eff m, Show b, Num b, MonadIO m) => b -> b -> m b
addBreak x y =
  Trace.withScope "Prelude.add" [show x, show y] $ do
    Trace.stackTrace
    pure (x + y)

multiply :: (Trace.Eff m, Show b, Num b) => b -> b -> m b
multiply x y =
  Trace.withScope "Prelude.multiply" [show x, show y] $ do
    pure (x * y)

formula :: (Trace.Eff m, Show b, MonadIO m, Fractional b) => b -> b -> m b
formula x y =
  Trace.withScope "multiply-add" [show x, show y] $ do
    addSquares <- addBreak (x ^ 2) (y ^ 2)
    pure (addSquares / 2)

complexFormula :: (Trace.Eff m, Show b, MonadIO m, Fractional b, Enum b) => b -> b -> m b
complexFormula x y =
  Trace.withScope "complex-formula" [show x, show y] $ do
    mult <- multiply x y
    aded <- add2 mult
    multiply aded mult

factorialBreak ::
  (Trace.Eff m, Show t, Eq t, Num t, HasThrow "error" Trace.Error m) => t -> t -> m t
factorialBreak x y =
  Trace.withScope "factorial" [show x, show y] $ do
    if
        | x == 0 ->
          Trace.break
        | otherwise -> do
          mult <- multiply x y
          factorial (x - 1) mult

factorial ::
  (Trace.Eff m, Show t, Eq t, Num t) => t -> t -> m t
factorial x y =
  Trace.withScope "factorial" [show x, show y] $ do
    if
        | x == 0 ->
          pure y
        | otherwise -> do
          mult <- multiply x y
          factorial (x - 1) mult

example1 :: IO ()
example1 = do
  (_, t) <- Env.runEmptyIO (Trace.traceAllEff >> complexFormula (3 :: Double) 4)
  Trace.info (Env.trace t)

example2 :: IO ()
example2 =
  ["factorial", "Prelude.multiply"]
    |> Trace.enable (Env.trace (snd (Env.runEmpty (factorial (5 :: Integer) 1))))
    |> Trace.info

example3 :: IO ()
example3 = do
  _ <- Env.runEmptyIO (formula (3 :: Float) 4)
  pure ()
