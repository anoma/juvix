module Juvix.Prelude.Effects.Resource
  ( module Juvix.Prelude.Effects.Resource,
    module Juvix.Prelude.Effects.MyResource,
  )
where

import Juvix.Prelude.Base.Foundation
import Juvix.Prelude.Effects.Base
import Juvix.Prelude.Effects.MyResource

finally ::
  (Member Resource r) =>
  -- | computation to run first
  Sem r a ->
  -- | computation to run afterward (even if an exception was raised)
  Sem r b ->
  Sem r a
finally act end = bracket (pure ()) (const end) (const act)

onException ::
  (Member Resource r) =>
  -- | computation to run first
  Sem r a ->
  -- | computation to run afterward if an exception was raised
  Sem r b ->
  Sem r a
onException act end = bracketOnError (pure ()) (const end) (const act)
