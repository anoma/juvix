module Main where

import Compile qualified
import Juvix.Prelude

main :: IO ()
main = do
  Compile.compile
