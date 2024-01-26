{-# LANGUAGE QuasiQuotes #-}

module Nockma.Compile.T1 where

import Data.String.Interpolate (__i)
import Juvix.Compiler.Nockma.Translation.FromSource
import Juvix.Prelude.Base

t1 :: Term Natural
t1 = parseOrCrash t1Src

t1Src :: Text
t1Src =
  [__i|
[
  0
  0
]
 |]
