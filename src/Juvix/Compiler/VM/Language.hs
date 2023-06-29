module Juvix.Compiler.VM.Language where

import Juvix.Compiler.VM.Language.Base

type SmallInt = Int

type RegRef = Int

data Value
  = Const SmallInt
  | RegRef RegRef

-- Constructor representation: tag, field1, .., fieldn
--
-- Closure representation: addr, n, m, arg1, .., argn
--
-- Here `m` is the total number of arguments the function accepts and `n` is the
-- number of arguments actually stored in the closure.
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
  | -- | JVB opcode: 'label labelName'
    Label InstrLabel

data BinaryOp = BinaryOp
  { _binaryOpCode :: Opcode,
    _binaryOpResult :: RegRef,
    _binaryOpArg1 :: Value,
    _binaryOpArg2 :: Value
  }

data InstrLoad = InstrLoad
  { _instrLoadDest :: RegRef,
    _instrLoadSrc :: RegRef,
    _instrLoadOffset :: SmallInt
  }

data InstrStore = InstrStore
  { _instrStoreDest :: RegRef,
    _instrStoreOffset :: SmallInt,
    _instrStoreValue :: Value
  }

data InstrMove = InstrMove
  { _instrMoveDest :: RegRef,
    _instrMoveValue :: Value
  }

data InstrAlloc = InstrAlloc
  { _instrAllocDest :: RegRef,
    _instrAllocNum :: SmallInt
  }

newtype InstrPush = InstrPush
  { _instrPushSrc :: RegRef
  }

newtype InstrPop = InstrPop
  { _instrPopDest :: RegRef
  }

newtype InstrJump = InstrJump
  { _instrJumpAddr :: SmallInt
  }

data InstrJumpOnZero = InstrJumpOnZero
  { _instrJumpOnZeroReg :: RegRef,
    _instrJumpOnZeroAddr :: SmallInt
  }

data InstrLabel = InstrLabel
  { _instrLabelName :: Text,
    _instrLabelSymbol :: Symbol
  }

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
