module Juvix.Compiler.Nockma.StdlibFunction.Base where

import Juvix.Prelude hiding (Atom, Path)
import Juvix.Prelude.Pretty

instance Pretty StdlibFunction where
  pretty = \case
    StdlibDec -> "dec"
    StdlibAdd -> "add"
    StdlibSub -> "sub"
    StdlibMul -> "mul"
    StdlibDiv -> "div"
    StdlibMod -> "mod"
    StdlibLt -> "<"
    StdlibLe -> "<="
    StdlibPow2 -> "pow2"
    StdlibEncode -> "encode"
    StdlibDecode -> "decode"

data StdlibFunction
  = StdlibDec
  | StdlibAdd
  | StdlibSub
  | StdlibMul
  | StdlibDiv
  | StdlibMod
  | StdlibLt
  | StdlibLe
  | StdlibPow2
  | StdlibEncode
  | StdlibDecode
  deriving stock (Show, Lift, Eq, Bounded, Enum, Generic)

instance Hashable StdlibFunction

instance NFData StdlibFunction
