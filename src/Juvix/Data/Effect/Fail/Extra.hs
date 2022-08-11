module Juvix.Data.Effect.Fail.Extra
  ( module Juvix.Data.Effect.Fail.Extra,
    module Juvix.Data.Effect.Fail,
  )
where

import Juvix.Data.Effect.Fail
import Juvix.Prelude.Base
import Juvix.Prelude.Base qualified as Prelude

last :: Members '[Fail] r => [a] -> Sem r a
last = failMaybe . fmap Prelude.last . nonEmpty

fromRight :: Members '[Fail] s => Either l r -> Sem s r
fromRight = \case
  Left {} -> fail
  Right r -> return r
