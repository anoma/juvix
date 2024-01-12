module Juvix.Compiler.Casm.Language
  ( module Juvix.Compiler.Casm.Language.Base,
    module Juvix.Compiler.Casm.Language,
  )
where

{-

This module defines data structures for an extended subset of the Cairo Assembly
language, following Section 5 of [1]. Except the `ExtraBinop` instruction, all
instructions correspond to the instructions from [1]. The parser and pretty
printer implemented in `Juvix.Compiler.Casm.Translation.FromSource` and
`Juvix.Compiler.Casm.Pretty` follow the syntax of [1, Section 5].

[1] Goldberg, Papini, Riabzev: "Cairo – a Turing-complete STARK-friendly CPU
    architecture" (https://ia.cr/2021/1063)

-}

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
  | IntAdd
  | IntSub
  | IntMul
  | IntDiv
  | IntMod
  | -- | Sets the result to 1 if arg1 < arg2, or to 0 otherwise
    IntLt

data Instruction
  = Assign InstrAssign
  | -- | Extra binary operation not directly available in Cairo Assembly bytecode,
    -- but easily encodable.
    ExtraBinop InstrExtraBinop
  | Jump InstrJump
  | JumpIf InstrJumpIf
  | JumpRel InstrJumpRel
  | Call InstrCall
  | Return
  | Alloc InstrAlloc
  | Trace InstrTrace
  | Label LabelRef

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
  { _instrJumpTarget :: Value,
    _instrJumpIncAp :: Bool
  }

-- | Jump if value is nonzero
data InstrJumpIf = InstrJumpIf
  { _instrJumpIfTarget :: Value,
    _instrJumpIfValue :: MemRef,
    _instrJumpIfIncAp :: Bool
  }

data InstrJumpRel = InstrJumpRel
  { _instrJumpRelTarget :: RValue,
    _instrJumpRelIncAp :: Bool
  }

newtype InstrCall = InstrCall
  { _instrCallTarget :: Value
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
makeLenses ''InstrJumpRel
makeLenses ''InstrCall
makeLenses ''InstrAlloc
makeLenses ''InstrTrace
