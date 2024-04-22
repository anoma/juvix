module Juvix.Compiler.Reg.Language.Blocks
  ( module Juvix.Compiler.Reg.Language.Blocks,
    module Juvix.Compiler.Reg.Language.Instrs,
  )
where

import Juvix.Compiler.Reg.Language.Instrs

data Block = Block
  { _blockLiveVars :: HashSet VarRef,
    _blockBody :: [Instruction],
    _blockFinal :: Maybe FinalInstruction,
    _blockNext :: Maybe Block
  }

data Instruction
  = Binop InstrBinop
  | Unop InstrUnop
  | Cairo InstrCairo
  | Assign InstrAssign
  | Alloc InstrAlloc
  | AllocClosure InstrAllocClosure
  | Trace InstrTrace
  | Dump
  | Failure InstrFailure
  deriving stock (Eq)

data FinalInstruction
  = ExtendClosure InstrExtendClosure
  | Call InstrCall
  | TailCall InstrTailCall
  | Return InstrReturn
  | Branch InstrBranch
  | Case InstrCase

type InstrBranch = InstrBranch' Block

type InstrCase = InstrCase' Block

type CaseBranch = CaseBranch' Block

makeLenses ''Block

emptyBlock :: Block
emptyBlock =
  Block
    { _blockLiveVars = mempty,
      _blockBody = [],
      _blockFinal = Nothing,
      _blockNext = Nothing
    }
