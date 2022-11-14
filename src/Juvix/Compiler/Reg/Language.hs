module Juvix.Compiler.Reg.Language
  ( module Juvix.Compiler.Reg.Language,
    module Juvix.Compiler.Reg.Language.Base,
  )
where

import Juvix.Compiler.Reg.Language.Base

data Value
  = ConstInt Integer
  | ConstBool Bool
  | ConstString Text
  | ConstUnit
  | ConstVoid
  | CRef ConstrField
  | VRef VarRef

type Index = Int

-- | Reference to a constructor field (argument).
data ConstrField = ConstrField
  { -- | Tag of the constructor being referenced.
    _constrFieldTag :: Tag,
    -- | Memory representation of the constructor.
    _constrFieldMemRep :: MemRep,
    -- | Location where the data is stored.
    _constrFieldRef :: VarRef,
    _constrFieldIndex :: Index
  }

data VarGroup = VarGroupArgs | VarGroupStack | VarGroupTemp

data VarRef = VarRef
  { _varRefGroup :: VarGroup,
    _varRefIndex :: Index
  }

data Instruction
  = Nop -- no operation
  | Binop BinaryOp
  | Assign InstrAssign
  | Trace InstrTrace
  | Dump
  | Failure InstrFailure
  | Prealloc InstrPrealloc
  | Alloc InstrAlloc
  | AllocClosure InstrAllocClosure
  | ExtendClosure InstrExtendClosure
  | Call InstrCall
  | CallClosures InstrCallClosures
  | Return InstrReturn
  | Branch InstrBranch
  | Case InstrCase

type Code = [Instruction]

data BinaryOp = BinaryOp
  { _binaryOpCode :: Opcode,
    _binaryOpResult :: VarRef,
    _binaryOpArg1 :: Value,
    _binaryOpArg2 :: Value
  }

data Opcode
  = OpIntAdd
  | OpIntSub
  | OpIntMul
  | OpIntDiv
  | OpIntMod
  | OpIntLt
  | OpIntLe
  | OpEq

data InstrAssign = InstrAssign
  { _instrAssignResult :: VarRef,
    _instrAssignValue :: Value
  }

newtype InstrTrace = InstrTrace
  { _instrTraceValue :: Value
  }

newtype InstrFailure = InstrFailure
  { _instrFailure :: Value
  }

data InstrPrealloc = InstrPrealloc
  { _instrPreallocWordsNum :: Int,
    _instrPreallocLiveVars :: [VarRef]
  }

data InstrAlloc = InstrAlloc
  { _instrAllocResult :: VarRef,
    _instrAllocTag :: Tag,
    _instrAllocMemRep :: MemRep,
    _instrAllocArgs :: [Value]
  }

data InstrAllocClosure = InstrAllocClosure
  { _instrAllocClosureResult :: VarRef,
    _instrAllocClosureSymbol :: Symbol,
    _instrAllocClosureExpectedArgsNum :: Int,
    _instrAllocClosureArgs :: [Value]
  }

data InstrExtendClosure = InstrExtendClosure
  { _instrExtendClosureResult :: VarRef,
    _instrExtendClosureValue :: VarRef,
    _instrExtendClosureArgs :: [Value]
  }

data CallType = CallFun Symbol | CallClosure VarRef

data InstrCall = InstrCall
  { _instrCallResult :: VarRef,
    _instrCallType :: CallType,
    _instrCallIsTail :: Bool,
    _instrCallArgs :: [Value],
    -- | Variables live at the point of the call. If the call is not
    -- tail-recursive, live variables need to be saved before the call and
    -- restored after it.
    _instrCallLiveVars :: [VarRef]
  }

data InstrCallClosures = InstrCallClosures
  { _instrCallClosuresResult :: VarRef,
    _instrCallClosuresIsTail :: Bool,
    _instrCallClosuresValue :: VarRef,
    _instrCallClosuresArgs :: [Value],
    _instrCallClosuresLiveVars :: [VarRef]
  }

data InstrBranch = InstrBranch
  { _instrBranchValue :: Value,
    _instrBranchTrue :: Code,
    _instrBranchFalse :: Code
  }

data InstrCase = InstrCase
  { _instrCaseValue :: Value,
    _instrCaseInductive :: Symbol,
    _instrCaseIndRep :: IndRep,
    _instrCaseBranches :: [CaseBranch],
    _instrCaseDefault :: Maybe Code
  }

data CaseBranch = CaseBranch
  { _caseBranchTag :: Tag,
    -- | Memory representation of the constructor corresponding to the branch.
    _caseBranchMemRep :: MemRep,
    _caseBranchArgsNum :: Int,
    _caseBranchCode :: Code
  }

newtype InstrReturn = InstrReturn
  { _instrReturnValue :: Value
  }

makeLenses ''ConstrField
makeLenses ''BinaryOp
makeLenses ''InstrAssign
makeLenses ''InstrTrace
makeLenses ''InstrFailure
makeLenses ''InstrPrealloc
makeLenses ''InstrAlloc
makeLenses ''InstrAllocClosure
makeLenses ''InstrExtendClosure
makeLenses ''InstrCall
makeLenses ''InstrCallClosures
makeLenses ''InstrBranch
makeLenses ''InstrCase
makeLenses ''CaseBranch
makeLenses ''InstrReturn
