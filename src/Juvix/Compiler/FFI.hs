module Juvix.Compiler.FFI where

import Juvix.Compiler.FFI.Anoma
import Juvix.Prelude

data FFI
  = FFIAnoma Anoma
  deriving stock (Generic, Show)

instance NFData FFI

instance Serialize FFI
