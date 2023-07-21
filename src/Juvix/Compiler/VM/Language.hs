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
  | MemRef RegRef
  | VarRef Text
  | LabelRef Text
  deriving stock (Show)

data LValue
  = LRegRef RegRef
  | LMemRef RegRef
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
  | -- | JVB opcode: 'jumpz reg, val'.
    JumpOnZero InstrJumpOnZero
  | -- | JVB opcode: 'halt'.
    Halt
  | -- | JVB opcode: 'move reg, val'.
    Move InstrMove
  | -- | JVB opcode: 'jump val'.
    Jump InstrJump
  | -- | JVB opcode: 'labelName:'
    Label InstrLabel
  deriving stock (Show)

data BinaryOp = BinaryOp
  { _binaryOpCode :: Opcode,
    _binaryOpResult :: LValue,
    _binaryOpArg1 :: Value,
    _binaryOpArg2 :: Value
  }
  deriving stock (Show)

data InstrMove = InstrMove
  { _instrMoveDest :: LValue,
    _instrMoveValue :: Value
  }
  deriving stock (Show)

newtype InstrJump = InstrJump
  { _instrJumpDest :: Value
  }
  deriving stock (Show)

data InstrJumpOnZero = InstrJumpOnZero
  { _instrJumpOnZeroValue :: Value,
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

makeLenses ''BinaryOp
makeLenses ''InstrMove
makeLenses ''InstrJump
makeLenses ''InstrJumpOnZero
makeLenses ''InstrLabel
