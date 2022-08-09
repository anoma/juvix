module Juvix.Extra.Process where

import Juvix.Prelude
import System.Info

openCmd :: Maybe String
openCmd = case os of
  "darwin" -> Just "open"
  "linux" -> Just "xdg-open"
  _ -> Nothing
