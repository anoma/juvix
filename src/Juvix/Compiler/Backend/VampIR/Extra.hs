module Juvix.Compiler.Backend.VampIR.Extra where

import Juvix.Prelude

getVampIRInputs :: Int -> [Maybe Text] -> [Text]
getVampIRInputs n argnames = zipWith fromMaybe args (argnames ++ repeat Nothing)
  where
    args :: [Text]
    args = if n == 1 then ["in"] else map (\k -> "in" <> show k) [1 .. n]
