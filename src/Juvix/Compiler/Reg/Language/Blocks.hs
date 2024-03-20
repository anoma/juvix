module Juvix.Compiler.Reg.Language.Blocks
  ( module Juvix.Compiler.Reg.Language.Blocks,
    module Juvix.Compiler.Reg.Language.Instrs,
  )
where

import Juvix.Compiler.Reg.Language.Instrs

data Block = Block
  { _blockLiveVars :: [VarRef],
    _blockBody :: [Instruction],
    _blockFinal :: Maybe FinalInstruction,
    _blockNext :: Maybe Block
  }

data Instruction
  = Binop InstrBinop
  | Unop InstrUnop
  | Assign InstrAssign
  | Alloc InstrAlloc
  | AllocClosure InstrAllocClosure
  | ExtendClosure InstrExtendClosure
  | Trace InstrTrace
  | Dump
  | Failure InstrFailure
  deriving stock (Eq)

data FinalInstruction
  = Call InstrCall
  | TailCall InstrTailCall
  | Return InstrReturn
  | Branch InstrBranch
  | Case InstrCase

type InstrBranch = InstrBranch' Block

type InstrCase = InstrCase' Block

makeLenses ''Block

emptyBlock :: Block
emptyBlock =
  Block
    { _blockLiveVars = [],
      _blockBody = [],
      _blockFinal = Nothing,
      _blockNext = Nothing
    }
