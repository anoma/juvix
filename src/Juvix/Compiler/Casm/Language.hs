module Juvix.Compiler.Casm.Language
  ( module Juvix.Compiler.Casm.Language.Base,
    module Juvix.Compiler.Casm.Language,
  )
where

{-

This module defines data structures for an extended subset of the Cairo Assembly
language, following Section 5 of [1]. Except for the `ExtraBinop` instruction
and absolute conditional jumps, all instructions correspond to the instructions
from [1]. The parser and pretty printer implemented in
`Juvix.Compiler.Casm.Translation.FromSource` and `Juvix.Compiler.Casm.Pretty`
follow the syntax of [1, Section 5].

[1] Goldberg, Papini, Riabzev: "Cairo – a Turing-complete STARK-friendly CPU
    architecture" (https://ia.cr/2021/1063)

-}

import Juvix.Compiler.Casm.Language.Base

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

data LoadValue = LoadValue
  { _loadValueSrc :: MemRef,
    _loadValueOff :: Offset
  }

data Opcode
  = FieldAdd
  | FieldMul

data BinopValue = BinopValue
  { _binopValueOpcode :: Opcode,
    _binopValueArg1 :: MemRef,
    _binopValueArg2 :: Value
  }

data RValue
  = Val Value
  | Load LoadValue
  | Binop BinopValue

-- | Extra opcodes that are not directly present in Cairo Assembly bytecode, but
-- can be implemented. The extra operations, except `FieldSub`, can increase
-- `ap` arbitrarily.
data ExtraOpcode
  = FieldSub
  | FieldDiv
  | IntAdd
  | IntSub
  | IntMul
  | IntDiv
  | IntMod
  | -- | Sets the result to zero if arg1 < arg2, or to non-zero otherwise
    IntLt

data Instruction
  = Assign InstrAssign
  | -- | Extra binary operation not directly available in Cairo Assembly bytecode,
    -- but easily encodable.
    ExtraBinop InstrExtraBinop
  | Jump InstrJump
  | JumpIf InstrJumpIf
  | Call InstrCall
  | Return
  | Alloc InstrAlloc
  | Trace InstrTrace
  | Label LabelRef
  | Nop

data InstrAssign = InstrAssign
  { _instrAssignValue :: RValue,
    _instrAssignResult :: MemRef,
    _instrAssignIncAp :: Bool
  }

data InstrExtraBinop = InstrExtraBinop
  { _instrExtraBinopArg1 :: MemRef,
    _instrExtraBinopArg2 :: Value,
    _instrExtraBinopResult :: MemRef,
    _instrExtraBinopOpcode :: ExtraOpcode,
    _instrExtraBinopIncAp :: Bool
  }

data InstrJump = InstrJump
  { _instrJumpTarget :: RValue,
    _instrJumpRel :: Bool,
    _instrJumpIncAp :: Bool
  }

-- | Jump if value is nonzero. Boolean true is translated to zero, false to
-- non-zero.
data InstrJumpIf = InstrJumpIf
  { _instrJumpIfTarget :: Value,
    _instrJumpIfValue :: MemRef,
    _instrJumpIfIncAp :: Bool
  }

data InstrCall = InstrCall
  { _instrCallTarget :: Value,
    _instrCallRel :: Bool
  }

newtype InstrAlloc = InstrAlloc
  { _instrAllocSize :: RValue
  }

newtype InstrTrace = InstrTrace
  { _instrTraceValue :: RValue
  }

type Code = [Instruction]

makeLenses ''MemRef
makeLenses ''LabelRef
makeLenses ''BinopValue
makeLenses ''LoadValue
makeLenses ''InstrAssign
makeLenses ''InstrExtraBinop
makeLenses ''InstrJump
makeLenses ''InstrJumpIf
makeLenses ''InstrCall
makeLenses ''InstrAlloc
makeLenses ''InstrTrace
