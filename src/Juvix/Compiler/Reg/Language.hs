module Juvix.Compiler.Reg.Language
  ( module Juvix.Compiler.Reg.Language,
    module Juvix.Compiler.Reg.Language.Instrs,
  )
where

import Juvix.Compiler.Reg.Language.Instrs

data Instruction
  = Binop InstrBinop
  | Unop InstrUnop
  | Cairo InstrCairo
  | Assign InstrAssign
  | Alloc InstrAlloc
  | AllocClosure InstrAllocClosure
  | ExtendClosure InstrExtendClosure
  | Call InstrCall
  | CallClosures InstrCallClosures
  | ----
    TailCall InstrTailCall
  | TailCallClosures InstrTailCallClosures
  | Return InstrReturn
  | ----
    Branch InstrBranch
  | Case InstrCase
  | ----
    Trace InstrTrace
  | Dump
  | Failure InstrFailure
  | Prealloc InstrPrealloc
  | Nop -- no operation
  | Block InstrBlock
  deriving stock (Eq)

type Code = [Instruction]

data InstrPrealloc = InstrPrealloc
  { _instrPreallocWordsNum :: Int,
    _instrPreallocLiveVars :: [VarRef]
  }
  deriving stock (Eq)

data InstrCallClosures = InstrCallClosures
  { _instrCallClosuresResult :: VarRef,
    _instrCallClosuresValue :: VarRef,
    _instrCallClosuresArgs :: [Value],
    _instrCallClosuresLiveVars :: [VarRef]
  }
  deriving stock (Eq)

data InstrTailCallClosures = InstrTailCallClosures
  { _instrTailCallClosuresValue :: VarRef,
    _instrTailCallClosuresArgs :: [Value]
  }
  deriving stock (Eq)

type InstrBranch = InstrBranch' Code

type InstrCase = InstrCase' Code

type CaseBranch = CaseBranch' Code

newtype InstrBlock = InstrBlock
  { _instrBlockCode :: Code
  }
  deriving stock (Eq)

makeLenses ''InstrPrealloc
makeLenses ''InstrCallClosures
makeLenses ''InstrTailCallClosures
