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

data VarGroup
  = VarGroupArgs
  | VarGroupLocal
  deriving stock (Eq, Generic)

instance Hashable VarGroup

data VarRef = VarRef
  { _varRefGroup :: VarGroup,
    _varRefIndex :: Index,
    _varRefName :: Maybe Text
  }

makeLenses ''VarRef
makeLenses ''ConstrField

instance Hashable VarRef where
  hashWithSalt salt VarRef {..} = hashWithSalt salt (_varRefGroup, _varRefIndex)

instance Eq VarRef where
  vr1 == vr2 =
    vr1 ^. varRefGroup == vr2 ^. varRefGroup
      && vr1 ^. varRefIndex == vr2 ^. varRefIndex

deriving stock instance (Eq ConstrField)

deriving stock instance (Eq Value)

data Instruction
  = Binop BinaryOp
  | Show InstrShow
  | StrToInt InstrStrToInt
  | Assign InstrAssign
  | ArgsNum InstrArgsNum
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
    -- | Variables live after the call. Live variables need to be
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
    _instrBranchFalse :: Code,
    -- | Output variable storing the result (corresponds to the top of the value
    -- stack in JuvixAsm after executing the branches)
    _instrBranchOutVar :: Maybe VarRef
  }
  deriving stock (Eq)

data InstrCase = InstrCase
  { _instrCaseValue :: Value,
    _instrCaseInductive :: Symbol,
    _instrCaseIndRep :: IndRep,
    _instrCaseBranches :: [CaseBranch],
    _instrCaseDefault :: Maybe Code,
    _instrCaseOutVar :: Maybe VarRef
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
makeLenses ''InstrShow
makeLenses ''InstrStrToInt
makeLenses ''InstrArgsNum
makeLenses ''InstrTailCall

mkVarRef :: VarGroup -> Index -> VarRef
mkVarRef g i =
  VarRef
    { _varRefGroup = g,
      _varRefIndex = i,
      _varRefName = Nothing
    }
