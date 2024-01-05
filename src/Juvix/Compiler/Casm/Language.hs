module Juvix.Compiler.Casm.Language
  ( module Juvix.Compiler.Casm.Language.Base,
    module Juvix.Compiler.Casm.Language,
  )
where

import Juvix.Compiler.Casm.Language.Base

type Offset = Int16

type Address = Int

data Reg
  = Ap
  | Fp

data MemRef = MemRef
  { _memRefReg :: Reg,
    _memRefOff :: Offset
  }

data LabelRef = LabelRef
  { _labelRefSymbol :: Symbol,
    _labelRefName :: Maybe Text
  }

type Immediate = Integer

data Value
  = -- | Immediate constant
    Imm Immediate
  | -- | Indirect memory reference
    Ref MemRef
  | -- | Label reference (translated to immediate in Cairo bytecode)
    Lab LabelRef

data RValue
  = Val Value
  | Load LoadValue
  | Binop BinopValue

data Opcode
  = FieldAdd
  | FieldSub
  | FieldMul

data BinopValue = BinopValue
  { _binopValueOpcode :: Opcode,
    _binopValueArg1 :: MemRef,
    _binopValueArg2 :: Value
  }

data LoadValue = LoadValue
  { _loadValueSrc :: MemRef,
    _loadValueOff :: Offset
  }

data Instruction
  = Assign InstrAssign
  | Jump InstrJump
  | JumpIf InstrJumpIf
  | Call InstrCall
  | Return
  | Alloc InstrAlloc
  | Label LabelRef

data InstrAssign = InstrAssign
  { _instrAssignValue :: RValue,
    _instrAssignResult :: MemRef,
    _instrAssignIncAp :: Bool
  }

data InstrJump = InstrJump
  { _instrJumpTarget :: Value,
    _instrJumpIncAp :: Bool
  }

-- | Jump if value is nonzero
data InstrJumpIf = InstrJumpIf
  { _instrJumpIfTarget :: Value,
    _instrJumpIfValue :: MemRef,
    _instrJumpIfIncAp :: Bool
  }

newtype InstrCall = InstrCall
  { _instrCallTarget :: Value
  }

newtype InstrAlloc = InstrAlloc
  { _instrAllocSize :: Int
  }

type Code = [Instruction]

makeLenses ''MemRef
makeLenses ''LabelRef
makeLenses ''BinopValue
makeLenses ''LoadValue
makeLenses ''InstrAssign
makeLenses ''InstrJump
makeLenses ''InstrJumpIf
makeLenses ''InstrCall
makeLenses ''InstrAlloc
