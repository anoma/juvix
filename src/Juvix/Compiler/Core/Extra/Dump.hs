module Juvix.Compiler.Core.Extra.Dump
  ( Dumper,
    module Juvix.Compiler.Core.Extra.Dump,
  )
where

import Juvix.Compiler.Core.Data.Module
import Juvix.Compiler.Core.Options
import Juvix.Compiler.Verification.Core.Dump qualified as Dump
import Juvix.Compiler.Verification.Dumper (Dumper)
import Juvix.Prelude

dump ::
  (Members '[Dumper, Reader CoreOptions] r) =>
  Text ->
  Module ->
  Sem r Module
dump phase md = do
  v <- asks (^. optVerify)
  if
    | v -> Dump.dump' phase md
    | otherwise -> return md
