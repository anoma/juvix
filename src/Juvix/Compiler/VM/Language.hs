module Juvix.Compiler.VM.Language
  ( module Juvix.Compiler.VM.Language,
    module Juvix.Compiler.VM.Language.Base,
  )
where

import Juvix.Compiler.VM.Language.Base hiding (Const)

type SmallInt = Int

type RegRef = Int

data Value
  = Const SmallInt
  | RegRef RegRef
  | VarRef Text
  | LabelRef Text
  deriving stock (Show)

-- Constructor representation: tag, field1, .., fieldn
--
-- Closure representation: addr, n, m, arg1, .., argn
--
-- Here `n` is the number of arguments stored in the closure and `m` is the
-- remaining number of arguments to the function.
--
-- The tag and the address can be read/written using ordinary load/store with
-- offset 0.

-- | Bytecode VM instructions.
data Instruction
  = Binop BinaryOp
  | -- | Loads src[offset] (heap value at `offset` relative to the value of
    -- register `src`) into register `dest`. JVB opcode: 'load dest, src,
    -- offset'.
    Load InstrLoad
  | -- | JVB opcode: 'store reg, offset, val'.
    Store InstrStore
  | -- | JVB opcode: 'move reg, val'.
    Move InstrMove
  | -- | JVB opcode: 'halt'.
    Halt
  | -- | Allocates `num` fields on the heap and stores the pointer in dest. JVB
    -- opcode: 'alloc reg, num'.
    Alloc InstrAlloc
  | -- | JVB opcode: 'push reg'.
    Push InstrPush
  | -- | JVB opcode: 'pop reg'.
    Pop InstrPop
  | -- | JVB opcode: 'jump val'.
    Jump InstrJump
  | -- | JVB opcode: 'jumpz reg, val'.
    JumpOnZero InstrJumpOnZero
  | -- | JVB opcode: 'labelName:'
    Label InstrLabel
  deriving stock (Show)

data BinaryOp = BinaryOp
  { _binaryOpCode :: Opcode,
    _binaryOpResult :: RegRef,
    _binaryOpArg1 :: Value,
    _binaryOpArg2 :: Value
  }
  deriving stock (Show)

data InstrLoad = InstrLoad
  { _instrLoadDest :: RegRef,
    _instrLoadSrc :: RegRef,
    _instrLoadOffset :: SmallInt
  }
  deriving stock (Show)

data InstrStore = InstrStore
  { _instrStoreDest :: RegRef,
    _instrStoreOffset :: SmallInt,
    _instrStoreValue :: Value
  }
  deriving stock (Show)

data InstrMove = InstrMove
  { _instrMoveDest :: RegRef,
    _instrMoveValue :: Value
  }
  deriving stock (Show)

data InstrAlloc = InstrAlloc
  { _instrAllocDest :: RegRef,
    _instrAllocSize :: Value
  }
  deriving stock (Show)

newtype InstrPush = InstrPush
  { _instrPushValue :: Value
  }
  deriving stock (Show)

newtype InstrPop = InstrPop
  { _instrPopDest :: RegRef
  }
  deriving stock (Show)

newtype InstrJump = InstrJump
  { _instrJumpDest :: Value
  }
  deriving stock (Show)

data InstrJumpOnZero = InstrJumpOnZero
  { _instrJumpOnZeroReg :: RegRef,
    _instrJumpOnZeroDest :: Value
  }
  deriving stock (Show)

newtype InstrLabel = InstrLabel
  { _instrLabelName :: Text
  }
  deriving stock (Show)

-- | Binary operation opcodes.
data Opcode
  = -- | JVB opcode: 'add reg, val1, val2'.
    OpIntAdd
  | -- | JVB opcode: 'sub reg, val1, val2'.
    OpIntSub
  | -- | JVB opcode: 'mul reg, val1, val2'.
    OpIntMul
  | -- | JVB opcode: 'div reg, val1, val2'.
    OpIntDiv
  | -- | JVB opcode: 'mod reg, val1, val2'.
    OpIntMod
  | -- | JVB opcode: 'lt reg, val1, val2'.
    OpIntLt
  | -- | JVB opcode: 'eq reg, val1, val2'.
    OpIntEq
  deriving stock (Show)

instructionOpcode :: Instruction -> Int
instructionOpcode = \case
  Binop BinaryOp {..} ->
    case _binaryOpCode of
      OpIntAdd -> 0
      OpIntSub -> 1
      OpIntMul -> 2
      OpIntDiv -> 3
      OpIntMod -> 4
      OpIntLt -> 5
      OpIntEq -> 6
  Load {} -> 7
  Store {} -> 8
  Move {} -> 9
  Halt -> 10
  Alloc {} -> 11
  Push {} -> 12
  Pop {} -> 13
  Jump {} -> 14
  JumpOnZero {} -> 15
  Label {} -> impossible

makeLenses ''BinaryOp
makeLenses ''InstrLoad
makeLenses ''InstrStore
makeLenses ''InstrMove
makeLenses ''InstrAlloc
makeLenses ''InstrPush
makeLenses ''InstrPop
makeLenses ''InstrJump
makeLenses ''InstrJumpOnZero
makeLenses ''InstrLabel
