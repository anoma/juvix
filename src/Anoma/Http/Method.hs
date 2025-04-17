module Anoma.Http.Method where

import Juvix.Prelude

data HttpMethod
  = HttpPost
  | HttpGet
  deriving stock (Eq, Show)
