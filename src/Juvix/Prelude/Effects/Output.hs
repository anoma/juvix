{-# OPTIONS_GHC -Wno-unused-type-patterns #-}
module Juvix.Prelude.Effects.Output where

import Data.Kind qualified as GHC
import Effectful.Dispatch.Dynamic
import Juvix.Prelude.Base hiding (Effect, Output, interpret, output, reinterpret, runOutputList)
import Juvix.Prelude.Effects.Accum
import Juvix.Prelude.Effects.Base
import System.Time.Extra

data Output (o :: GHC.Type) :: Effect where
  Output :: o -> Output o m ()

makeEffect ''Output

runOutputEff :: (o -> Eff r ()) -> Eff (Output o ': r) a -> Eff r a
runOutputEff handle =
  interpret $ \_ -> \case
    Output x -> handle x

runOutputList :: Eff (Output o ': r) a -> Eff r ([o], a)
runOutputList = reinterpret runAccumList $ \_ -> \case
  Output x -> accum x

ignoreOutput :: Eff (Output o ': r) a -> Eff r a
ignoreOutput = interpret $ \_ -> \case
  Output {} -> return ()

example1 :: IO ()
example1 =
  runEff $
    runOutputEff (\n -> putStrLn ("hey " <> show @Natural n)) (go 3)

go :: (Output Natural :> r, IOE :> r) => Natural -> Eff r ()
go = \case
  0 -> return ()
  n -> do
    output n
    liftIO (sleep 1)
    go (pred n)
