module Benchmark.Effect.EmbedIO where

import Juvix.Prelude
import Juvix.Prelude.Effects (Eff)
import Juvix.Prelude.Effects qualified as E
import Test.Tasty.Bench

bm :: Benchmark
bm =
  bgroup
    "Embed IO"
    [ bench "Raw IO" $ nfAppIO countRaw k,
      bench "Eff RIO" $ nfAppIO countEff k,
      bench "Sem Embed IO" $ nfAppIO countSem k
    ]

k :: Natural
k = 2 ^ (23 :: Natural)

c :: Char
c = 'x'

countRaw :: Natural -> IO ()
countRaw n =
  withSystemTempFile "tmp" $ \_ h -> go h n
  where
    go :: Handle -> Natural -> IO ()
    go h = \case
      0 -> return ()
      a -> hPutChar h c >> go h (pred a)

countSem :: Natural -> IO ()
countSem n = withSystemTempFile "tmp" $ \_ h -> runM (go h n)
  where
    go :: Handle -> Natural -> Sem '[Embed IO] ()
    go h = \case
      0 -> return ()
      a -> liftIO (hPutChar h c) >> go h (pred a)

countEff :: Natural -> IO ()
countEff n = withSystemTempFile "tmp" $ \_ h -> E.runEff (go h n)
  where
    go :: Handle -> Natural -> Eff '[E.IOE] ()
    go h = \case
      0 -> return ()
      a -> liftIO (hPutChar h c) >> go h (pred a)
