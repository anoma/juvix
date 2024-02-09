module Juvix.Compiler.Reg.Language
  ( module Juvix.Compiler.Reg.Language,
    module Juvix.Compiler.Reg.Language.Base,
  )
where

import Juvix.Compiler.Reg.Language.Base

data Value
  = Const Constant
  | CRef ConstrField
  | VRef VarRef
  deriving stock (Eq)

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
  deriving stock (Eq)

data VarGroup
  = VarGroupArgs
  | VarGroupLocal
  deriving stock (Eq)

data VarRef = VarRef
  { _varRefGroup :: VarGroup,
    _varRefIndex :: Index,
    _varRefName :: Maybe Text
  }
  deriving stock (Eq)

data Instruction
  = Nop -- no operation
  | Binop BinaryOp
  | Show InstrShow
  | StrToInt InstrStrToInt
  | Assign InstrAssign
  | Trace InstrTrace
  | Dump
  | Failure InstrFailure
  | ArgsNum InstrArgsNum
  | Prealloc InstrPrealloc
  | Alloc InstrAlloc
  | AllocClosure InstrAllocClosure
  | ExtendClosure InstrExtendClosure
  | Call InstrCall
  | TailCall InstrTailCall
  | CallClosures InstrCallClosures
  | TailCallClosures InstrTailCallClosures
  | Return InstrReturn
  | Branch InstrBranch
  | Case InstrCase
  | Block InstrBlock
  deriving stock (Eq)

type Code = [Instruction]

data BinaryOp = BinaryOp
  { _binaryOpCode :: Opcode,
    _binaryOpResult :: VarRef,
    _binaryOpArg1 :: Value,
    _binaryOpArg2 :: Value
  }
  deriving stock (Eq)

data Opcode
  = OpIntAdd
  | OpIntSub
  | OpIntMul
  | OpIntDiv
  | OpIntMod
  | OpIntLt
  | OpIntLe
  | OpEq
  | OpStrConcat
  deriving stock (Eq)

data InstrShow = InstrShow
  { _instrShowResult :: VarRef,
    _instrShowValue :: Value
  }
  deriving stock (Eq)

data InstrStrToInt = InstrStrToInt
  { _instrStrToIntResult :: VarRef,
    _instrStrToIntValue :: Value
  }
  deriving stock (Eq)

data InstrAssign = InstrAssign
  { _instrAssignResult :: VarRef,
    _instrAssignValue :: Value
  }
  deriving stock (Eq)

newtype InstrTrace = InstrTrace
  { _instrTraceValue :: Value
  }
  deriving stock (Eq)

newtype InstrFailure = InstrFailure
  { _instrFailureValue :: Value
  }
  deriving stock (Eq)

data InstrArgsNum = InstrArgsNum
  { _instrArgsNumResult :: VarRef,
    _instrArgsNumValue :: Value
  }
  deriving stock (Eq)

data InstrPrealloc = InstrPrealloc
  { _instrPreallocWordsNum :: Int,
    _instrPreallocLiveVars :: [VarRef]
  }
  deriving stock (Eq)

data InstrAlloc = InstrAlloc
  { _instrAllocResult :: VarRef,
    _instrAllocTag :: Tag,
    _instrAllocMemRep :: MemRep,
    _instrAllocArgs :: [Value]
  }
  deriving stock (Eq)

data InstrAllocClosure = InstrAllocClosure
  { _instrAllocClosureResult :: VarRef,
    _instrAllocClosureSymbol :: Symbol,
    _instrAllocClosureExpectedArgsNum :: Int,
    _instrAllocClosureArgs :: [Value]
  }
  deriving stock (Eq)

data InstrExtendClosure = InstrExtendClosure
  { _instrExtendClosureResult :: VarRef,
    _instrExtendClosureValue :: VarRef,
    _instrExtendClosureArgs :: [Value]
  }
  deriving stock (Eq)

data CallType
  = CallFun Symbol
  | CallClosure VarRef
  deriving stock (Eq)

data InstrCall = InstrCall
  { _instrCallResult :: VarRef,
    _instrCallType :: CallType,
    _instrCallArgs :: [Value],
    -- | Variables live at the point of the call. Live variables need to be
    -- saved before the call and restored after it.
    _instrCallLiveVars :: [VarRef]
  }
  deriving stock (Eq)

data InstrTailCall = InstrTailCall
  { _instrTailCallType :: CallType,
    _instrTailCallArgs :: [Value]
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

newtype InstrReturn = InstrReturn
  { _instrReturnValue :: Value
  }
  deriving stock (Eq)

data InstrBranch = InstrBranch
  { _instrBranchValue :: Value,
    _instrBranchTrue :: Code,
    _instrBranchFalse :: Code
  }
  deriving stock (Eq)

data InstrCase = InstrCase
  { _instrCaseValue :: Value,
    _instrCaseInductive :: Symbol,
    _instrCaseIndRep :: IndRep,
    _instrCaseBranches :: [CaseBranch],
    _instrCaseDefault :: Maybe Code
  }
  deriving stock (Eq)

data CaseBranch = CaseBranch
  { _caseBranchTag :: Tag,
    -- | Memory representation of the constructor corresponding to the branch.
    _caseBranchMemRep :: MemRep,
    _caseBranchArgsNum :: Int,
    _caseBranchCode :: Code
  }
  deriving stock (Eq)

newtype InstrBlock = InstrBlock
  { _instrBlockCode :: Code
  }
  deriving stock (Eq)

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

mkVarRef :: VarGroup -> Index -> VarRef
mkVarRef g i =
  VarRef
    { _varRefGroup = g,
      _varRefIndex = i,
      _varRefName = Nothing
    }
