{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HaskelineJH where

import Juvix.Prelude.DarkArts
import System.Console.Haskeline
import System.Console.Repline
import Unsafe.Coerce

coerceMany :: (a %1 -> b) -> a -> b
coerceMany f = f

unInputT :: InputT m a -> _
unInputT = $(importHidden "haskeline" "System.Console.Haskeline.InputT" "unInputT")

mkInputT :: _ -> InputT m a
mkInputT = coerceMany $(importHiddenCon "haskeline" "System.Console.Haskeline.InputT" "InputT")

unHaskelineT :: HaskelineT m a -> InputT m a
unHaskelineT = $(importHidden "repline" "System.Console.Repline" "unHaskeline")

mkHaskelineT :: InputT m a -> HaskelineT m a
mkHaskelineT = coerceMany $(importHiddenCon "repline" "System.Console.Repline" "HaskelineT")
