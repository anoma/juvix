module Juvix.Compiler.Nockma.AnomaLib.Base where

import Juvix.Prelude hiding (Atom, Path)
import Juvix.Prelude.Pretty

data AnomaFunction
  = AnomaStdlibFunction StdlibFunction
  | AnomaRmFunction RmFunction
  deriving stock (Show, Lift, Eq, Generic)

instance Hashable AnomaFunction

instance NFData AnomaFunction

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
  | StdlibRandomInitGen
  | StdlibRandomNextBits
  | StdlibRandomSplit
  | StdlibAnomaSetToList
  | StdlibAnomaSetFromList
  deriving stock (Show, Lift, Eq, Bounded, Enum, Generic)

instance Hashable StdlibFunction

instance NFData StdlibFunction

-- | Anoma Resource Machine client library functions
data RmFunction
  = RmCommit
  | RmNullify
  | RmKind
  | RmDeltaAdd
  | RmDeltaSub
  | RmResourceDelta
  | RmActionDelta
  | RmMakeDelta
  | RmIsCommitment
  | RmIsNullifier
  | RmCreateFromComplianceInputs
  | RmActionCreate
  | RmTransactionCompose
  deriving stock (Show, Lift, Eq, Bounded, Enum, Generic)

instance Hashable RmFunction

instance NFData RmFunction

newtype AnomaValue
  = AnomaRmValue RmValue
  deriving stock (Show, Lift, Eq, Generic)

instance Hashable AnomaValue

instance NFData AnomaValue

-- | Anoma Resource Machine client library values
data RmValue
  = RmZeroDelta
  deriving stock (Show, Lift, Eq, Bounded, Enum, Generic)

instance Hashable RmValue

instance NFData RmValue

data AnomaLib
  = AnomaLibFunction AnomaFunction
  | AnomaLibValue AnomaValue
  deriving stock (Show, Lift, Eq, Generic)

instance Hashable AnomaLib

instance NFData AnomaLib

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
    StdlibRandomInitGen -> "random-init"
    StdlibRandomNextBits -> "random-next-bits"
    StdlibRandomSplit -> "random-split"
    StdlibAnomaSetToList -> "set-to-list"
    StdlibAnomaSetFromList -> "set-from-list"

instance Pretty RmFunction where
  pretty = \case
    RmCommit -> "commit"
    RmNullify -> "nullify"
    RmKind -> "kind"
    RmDeltaAdd -> "delta-add"
    RmDeltaSub -> "delta-sub"
    RmResourceDelta -> "resource-delta"
    RmActionDelta -> "action-delta"
    RmMakeDelta -> "make-delta"
    RmIsCommitment -> "is-commitment"
    RmIsNullifier -> "is-nullifier"
    RmActionCreate -> "action-create"
    RmTransactionCompose -> "transaction-compose"
    RmCreateFromComplianceInputs -> "create-from-compliance-inputs"

instance Pretty RmValue where
  pretty = \case
    RmZeroDelta -> "zero-delta"
