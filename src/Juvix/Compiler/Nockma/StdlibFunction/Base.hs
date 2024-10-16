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
    StdlibVerifyDetached -> "verify-detached"
    StdlibSign -> "sign"
    StdlibSignDetached -> "sign-detached"
    StdlibVerify -> "verify"
    StdlibCatBytes -> "cat"
    StdlibFoldBytes -> "fold-bytes"
    StdlibLengthList -> "length-list"
    StdlibLengthBytes -> "length-bytes"
    StdlibCurry -> "curry"
    StdlibSha256 -> "sha256"

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
  | StdlibVerifyDetached
  | StdlibSign
  | StdlibSignDetached
  | StdlibVerify
  | StdlibCatBytes
  | StdlibFoldBytes
  | StdlibLengthList
  | StdlibLengthBytes
  | StdlibCurry
  | StdlibSha256
  deriving stock (Show, Lift, Eq, Bounded, Enum, Generic)

instance Hashable StdlibFunction

instance NFData StdlibFunction
